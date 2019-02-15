{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | Implementation of "Imm.Database" based on a JSON file.
module Imm.Database.JsonFile
  ( JsonFileDatabase
  , mkJsonFileDatabase
  , defaultDatabase
  , mkHandle
  , JsonException(..)
  , module Imm.Database.FeedTable
  ) where

-- {{{ Imports
import           Imm.Database                hiding (commit, delete, insert,
                                              purge, update)
import           Imm.Database.FeedTable
import           Imm.Error
import           Imm.Pretty

import           Control.Concurrent.STM.TVar (swapTVar)
import           Data.Aeson
import           Data.ByteString.Lazy        (hPut)
import           Data.ByteString.Streaming   (hGetContents, toLazy_)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import qualified Data.Set                    as Set
import           Streaming.With              hiding (withFile)
import           System.Directory
import           System.FilePath
-- }}}

data CacheStatus = Empty | Clean | Dirty
  deriving(Eq, Show)

data JsonFileDatabase t = JsonFileDatabase FilePath (Map (Key t) (Entry t)) CacheStatus

instance Pretty (JsonFileDatabase t) where
  pretty (JsonFileDatabase file _ _) = "JSON database: " <+> pretty file

mkJsonFileDatabase :: (Table t) => FilePath -> JsonFileDatabase t
mkJsonFileDatabase file = JsonFileDatabase file mempty Empty

-- | Default database is stored in @$XDG_CONFIG_HOME\/imm\/feeds.json@
defaultDatabase :: Table t => IO (TVar (JsonFileDatabase t))
defaultDatabase = do
  databaseFile <- getXdgDirectory XdgConfig "imm/feeds.json"
  newTVarIO $ mkJsonFileDatabase databaseFile


data JsonException = UnableDecode
  deriving(Eq, Show)

instance Exception JsonException where
  displayException _ = "Unable to parse JSON"


mkHandle :: (Table t, FromJSON (Key t), FromJSON (Entry t), ToJSON (Key t), ToJSON (Entry t), MonadIO m, MonadMask m)
         => TVar (JsonFileDatabase t) -> Handle m t
mkHandle tvar = Handle
  { _describeDatabase = pretty <$> readTVarIO tvar
  , _fetchList = \keys -> loadInCache tvar >> (Map.filterWithKey (\uri _ -> Set.member uri $ Set.fromList keys) <$> getCache tvar)
  , _fetchAll = loadInCache tvar >> getCache tvar
  , _update = \key f -> loadInCache tvar >> atomically (modifyTVar' tvar $ updateInCache key f)
  , _insertList = \list -> loadInCache tvar >> atomically (modifyTVar' tvar $ insertInCache list)
  , _deleteList = \list -> loadInCache tvar >> atomically (modifyTVar' tvar $ deleteInCache list)
  , _purge = loadInCache tvar >> atomically (modifyTVar' tvar purgeInCache)
  , _commit = commit tvar
  }

-- * Low-level implementation

loadInCache :: (Table t, FromJSON (Key t), FromJSON (Entry t), MonadIO m, MonadMask m) => TVar (JsonFileDatabase t) -> m ()
loadInCache tvar = do
  JsonFileDatabase file _ status <- readTVarIO tvar
  when (status == Empty) $ do
    database <- loadFromDisk file
    atomically $ do
      JsonFileDatabase _ _ status' <- readTVar tvar
      when (status' == Empty) $ writeTVar tvar database

loadFromDisk :: (Table t, FromJSON (Key t), FromJSON (Entry t), MonadIO m, MonadMask m) => FilePath -> m (JsonFileDatabase t)
loadFromDisk file = do
  liftIO $ createDirectoryIfMissing True $ takeDirectory file
  fileContent <- withBinaryFile file ReadWriteMode (toLazy_ . hGetContents)
  cache <- (`failWith` UnableDecode) $ fmap Map.fromList $ decode $ fromEmpty "[]" fileContent
  return $ JsonFileDatabase file cache Clean
  where fromEmpty x "" = x
        fromEmpty _ y  = y

getCache :: MonadIO m => TVar (JsonFileDatabase t) -> m (Map (Key t) (Entry t))
getCache tvar = do
  JsonFileDatabase _ cache _ <- readTVarIO tvar
  return cache


insertInCache :: Table t => [(Key t, Entry t)] -> JsonFileDatabase t -> JsonFileDatabase t
insertInCache rows (JsonFileDatabase file cache _) = JsonFileDatabase file (Map.union cache $ Map.fromList rows) Dirty

updateInCache :: Table t => Key t -> (Entry t -> Entry t) -> JsonFileDatabase t -> JsonFileDatabase t
updateInCache key f (JsonFileDatabase file cache _) = JsonFileDatabase file newCache Dirty where
  newCache = Map.update (Just . f) key cache

deleteInCache :: Table t => [Key t] -> JsonFileDatabase t -> JsonFileDatabase t
deleteInCache keys (JsonFileDatabase file cache _) = JsonFileDatabase file newCache Dirty where
  newCache = foldr Map.delete cache keys

purgeInCache :: Table t => JsonFileDatabase t -> JsonFileDatabase t
purgeInCache (JsonFileDatabase file _ _) = JsonFileDatabase file mempty Dirty

commit :: (ToJSON (Key t), ToJSON (Entry t), MonadIO m)
       => TVar (JsonFileDatabase t) -> m ()
commit tvar = do
  JsonFileDatabase file cache status <- atomically $ do
    database@(JsonFileDatabase file cache status) <- readTVar tvar
    when (status == Dirty) $ void $ swapTVar tvar $ JsonFileDatabase file cache Clean
    return database

  when (status == Dirty) $ liftIO $ withFile file WriteMode $ \h -> hPut h $ encode $ Map.toList cache
