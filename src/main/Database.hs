{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | Implementation of "Imm.Database.Feed" based on a JSON file.
module Database
  ( JsonFileDatabase
  , mkJsonFileDatabase
  , defaultDatabase
  , mkHandle
  , JsonException(..)
  , module Imm.Database.Feed
  ) where

-- {{{ Imports
import           Imm.Database.Feed           hiding (commit, delete1)
import           Imm.Feed
import           Imm.Pretty

import           Control.Concurrent.STM.TVar (swapTVar)
import           Control.Exception.Safe
import           Data.Aeson
import qualified Data.ByteString             as ByteString
import           Data.ByteString.Lazy        (hPut)
import qualified Data.Map                    as Map
import           System.Directory
import           System.FilePath
import           System.IO                   hiding (Handle)
-- }}}

data CacheStatus = Empty | Clean | Dirty
  deriving(Eq, Ord, Read, Show)

data JsonFileDatabase = JsonFileDatabase FilePath (Map FeedLocation Entry) CacheStatus

instance Pretty JsonFileDatabase where
  pretty (JsonFileDatabase file _ _) = "JSON database: " <+> pretty file

mkJsonFileDatabase :: FilePath -> JsonFileDatabase
mkJsonFileDatabase file = JsonFileDatabase file mempty Empty

-- | Default database is stored in @$XDG_CONFIG_HOME\/imm\/feeds.json@
defaultDatabase :: IO (TVar JsonFileDatabase)
defaultDatabase = do
  databaseFile <- getXdgDirectory XdgConfig "imm/feeds.json"
  newTVarIO $ mkJsonFileDatabase databaseFile


data JsonException = UnableDecode
  deriving(Eq, Show)

instance Exception JsonException where
  displayException _ = "Unable to parse JSON"


mkHandle :: (MonadIO m, MonadMask m)
         => TVar JsonFileDatabase -> Handle m
mkHandle tvar = Handle
  { _describeDatabase = pretty <$> readTVarIO tvar
  , _fetch = \keys -> do
      loadInCache tvar
      database <- readTVarIO tvar
      return $ Map.fromList $ do
        key <- keys
        entry <- maybeToList $ fetchFromCache key database
        return (key, entry)
  , _fetchAll = loadInCache tvar
      >> getCache tvar
      <&> Map.toList
      <&> map snd
      <&> zip [0..]
      <&> Map.fromList
  , _update = \key f -> loadInCache tvar >> atomically (modifyTVar' tvar $ updateInCache key f)
  , _insert = \entries -> do
      loadInCache tvar
      atomically $ do
        modifyTVar' tvar $ insertInCache entries
        database <- readTVar tvar
        return $ Map.fromList $ do
          location <- entries <&> entryLocation
          i <- maybeToList $ getIndex location database
          return (location, i)
  , _delete = \list -> loadInCache tvar >> atomically (modifyTVar' tvar $ deleteInCache list)
  , _purge = loadInCache tvar >> atomically (modifyTVar' tvar purgeInCache)
  , _commit = commit tvar
  }

-- * Low-level implementation

getIndex :: FeedLocation -> JsonFileDatabase -> Maybe Int
getIndex location (JsonFileDatabase _ cache _) = Map.lookupIndex location cache

loadInCache :: (MonadIO m, MonadMask m) => TVar JsonFileDatabase -> m ()
loadInCache tvar = do
  JsonFileDatabase file _ status <- readTVarIO tvar
  when (status == Empty) $ do
    database <- loadFromDisk file
    atomically $ do
      JsonFileDatabase _ _ status' <- readTVar tvar
      when (status' == Empty) $ writeTVar tvar database

loadFromDisk :: (MonadIO m, MonadMask m) => FilePath -> m JsonFileDatabase
loadFromDisk file = do
  liftIO $ createDirectoryIfMissing True $ takeDirectory file
  fileContent <- io $ withBinaryFile file ReadWriteMode ByteString.hGetContents
  cache <- fileContent
    & fromEmpty "[]"
    & decodeStrict
    & fmap Map.fromList
    & (`failWith` UnableDecode)
  return $ JsonFileDatabase file cache Clean
  where fromEmpty x "" = x
        fromEmpty _ y  = y

getCache :: MonadIO m => TVar JsonFileDatabase -> m (Map FeedLocation Entry)
getCache tvar = do
  JsonFileDatabase _ cache _ <- readTVarIO tvar
  return cache

fetchFromCache :: EntryKey -> JsonFileDatabase -> Maybe (Int, Entry)
fetchFromCache (ByLocation location) (JsonFileDatabase _ cache _) = (,)
  <$> Map.lookupIndex location cache
  <*> Map.lookup location cache
fetchFromCache (ById i) (JsonFileDatabase _ cache _) = if i >= 0 && i < Map.size cache
  then Just $ (i,) $ snd $ Map.elemAt i cache
  else Nothing


insertInCache :: [Entry] -> JsonFileDatabase -> JsonFileDatabase
insertInCache entries (JsonFileDatabase file cache _) = JsonFileDatabase file newCache Dirty where
  newCache = Map.union cache $ Map.fromList $ map (\entry -> (entryLocation entry, entry)) entries

updateInCache :: [EntryKey] -> (Entry -> Entry) -> JsonFileDatabase -> JsonFileDatabase
updateInCache keys f (JsonFileDatabase file cache _) = JsonFileDatabase file newCache Dirty where
  newCache = foldr update1 cache keys
  update1 (ByLocation location) = Map.update (Just . f) location
  update1 (ById i)              = Map.updateAt (const $ Just . f) i

deleteInCache :: [EntryKey] -> JsonFileDatabase -> JsonFileDatabase
deleteInCache keys (JsonFileDatabase file oldCache _) = JsonFileDatabase file newCache Dirty where
  newCache = foldr delete1 oldCache keys
  delete1 (ByLocation location) = Map.delete location
  delete1 (ById i)              = Map.deleteAt i

purgeInCache :: JsonFileDatabase -> JsonFileDatabase
purgeInCache (JsonFileDatabase file _ _) = JsonFileDatabase file mempty Dirty

commit :: (MonadIO m)
       => TVar JsonFileDatabase -> m ()
commit tvar = do
  JsonFileDatabase file cache status <- atomically $ do
    database@(JsonFileDatabase file cache status) <- readTVar tvar
    when (status == Dirty) $ void $ swapTVar tvar $ JsonFileDatabase file cache Clean
    return database

  when (status == Dirty) $ liftIO $ withFile file WriteMode $ \h -> hPut h $ encode $ Map.toList cache


-- | Wrap a 'Maybe' value in 'MonadThrow'
failWith :: (MonadThrow m, Exception e) => Maybe a -> e -> m a
failWith x e = maybe (throwM e) return x
