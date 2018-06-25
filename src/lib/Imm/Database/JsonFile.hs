{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | Implementation of "Imm.Database" based on a JSON file.
module Imm.Database.JsonFile
  ( JsonFileDatabase
  , mkJsonFileDatabase
  , defaultDatabase
  , JsonException(..)
  , module Imm.Database.FeedTable
  ) where

-- {{{ Imports
import           Imm.Database                   hiding (commit, delete, insert,
                                                 purge, update)
import           Imm.Database.FeedTable
import           Imm.Error
import           Imm.Prelude                    hiding (delete, keys)
import           Imm.Pretty

import           Control.Concurrent.MVar.Lifted
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.Reader     (ReaderT)
import           Data.Aeson
import           Data.ByteString.Lazy           (hPut)
import           Data.ByteString.Streaming      (hGetContents, toLazy_)
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import qualified Data.Set                       as Set
import           Streaming.With
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
defaultDatabase :: Table t => IO (MVar (JsonFileDatabase t))
defaultDatabase = do
  databaseFile <- getXdgDirectory XdgConfig "imm/feeds.json"
  newMVar $ mkJsonFileDatabase databaseFile


data JsonException = UnableDecode
  deriving(Eq, Show)

instance Exception JsonException where
  displayException _ = "Unable to parse JSON"


instance (Table t, FromJSON (Key t), FromJSON (Entry t), ToJSON (Key t), ToJSON (Entry t))
  => MonadDatabase t (ReaderT (MVar (JsonFileDatabase t)) IO) where
  _describeDatabase _ = pretty <$> (readMVar =<< ask)
  _fetchList t keys = Map.filterWithKey (\uri _ -> member uri $ Set.fromList keys) <$> fetchAll t
  _fetchAll _ = do
    mvar <- ask
    lift $ modifyMVar mvar $ \database -> do
      a@(JsonFileDatabase _ cache _) <- loadInCache database
      return (a, cache)
  _update _ key f = exec (\a -> update a key f)
  _insertList _ rows = exec $ insert rows
  _deleteList _ keys = exec $ delete keys
  _purge _ = exec purge
  _commit _ = exec commit

exec :: (a -> IO a) -> ReaderT (MVar a) IO ()
exec f = do
  mvar <- ask
  lift $ modifyMVar_ mvar f


-- * Low-level implementation

loadInCache :: (Table t, FromJSON (Key t), FromJSON (Entry t))
            => JsonFileDatabase t -> IO (JsonFileDatabase t)
loadInCache t@(JsonFileDatabase file _ status) = case status of
  Empty -> do
    createDirectoryIfMissing True $ takeDirectory file
    fileContent <- withBinaryFile file ReadWriteMode (toLazy_ . hGetContents)
    cache <- (`failWith` UnableDecode) $ fmap Map.fromList $ decode $ fromEmpty "[]" fileContent
    return $ JsonFileDatabase file cache Clean
  _ -> return t
  where fromEmpty x "" = x
        fromEmpty _ y  = y


insert :: (Table t, FromJSON (Key t), FromJSON (Entry t))
       => [(Key t, Entry t)] -> JsonFileDatabase t -> IO (JsonFileDatabase t)
insert rows t = insertInCache rows <$> loadInCache t

insertInCache :: Table t => [(Key t, Entry t)] -> JsonFileDatabase t -> JsonFileDatabase t
insertInCache rows (JsonFileDatabase file cache _) = JsonFileDatabase file (Map.union cache $ Map.fromList rows) Dirty


update :: (Table t, FromJSON (Key t), FromJSON (Entry t))
       => JsonFileDatabase t -> Key t -> (Entry t -> Entry t) -> IO (JsonFileDatabase t)
update t key f = updateInCache key f <$> loadInCache t

updateInCache :: Table t => Key t -> (Entry t -> Entry t) -> JsonFileDatabase t -> JsonFileDatabase t
updateInCache key f (JsonFileDatabase file cache _) = JsonFileDatabase file newCache Dirty where
  newCache = Map.update (Just . f) key cache

delete :: (Table t, FromJSON (Key t), FromJSON (Entry t))
       => [Key t] -> JsonFileDatabase t -> IO (JsonFileDatabase t)
delete keys t = deleteInCache keys <$> loadInCache t

deleteInCache :: Table t => [Key t] -> JsonFileDatabase t -> JsonFileDatabase t
deleteInCache keys (JsonFileDatabase file cache _) = JsonFileDatabase file newCache Dirty where
  newCache = foldr Map.delete cache keys

purge :: (Table t, FromJSON (Key t), FromJSON (Entry t))
      => JsonFileDatabase t -> IO (JsonFileDatabase t)
purge t = purgeInCache <$> loadInCache t

purgeInCache :: Table t => JsonFileDatabase t -> JsonFileDatabase t
purgeInCache (JsonFileDatabase file _ _) = JsonFileDatabase file mempty Dirty

commit :: (ToJSON (Key t), ToJSON (Entry t))
       => JsonFileDatabase t -> IO (JsonFileDatabase t)
commit t@(JsonFileDatabase file cache status) = case status of
  Dirty -> do
    withFile file WriteMode $ \h -> (hPut h $ encode $ Map.toList cache)
    return $ JsonFileDatabase file cache Clean
  _ -> return t
