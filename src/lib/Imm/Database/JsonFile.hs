{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | Database interpreter based on a JSON file
module Imm.Database.JsonFile (module Imm.Database.JsonFile, module Reexport) where

-- {{{ Imports
import           Imm.Database           hiding (commit, delete, fetchAll,
                                         insert, purge, update)
import           Imm.Database.FeedTable as Reexport
import           Imm.Error
import           Imm.Prelude            hiding (catch, delete, keys)

import           Data.Aeson
import qualified Data.Map               as Map
import qualified Data.Set               as Set

import           System.Directory
import           System.FilePath
import           System.IO              (IOMode (..), openFile)
-- }}}

-- * Types

data CacheStatus = Empty | Clean | Dirty
  deriving(Eq, Show)

data JsonFileDatabase t = JsonFileDatabase FilePath (Map (Key t) (Entry t)) CacheStatus

instance Pretty (JsonFileDatabase t) where
  pretty (JsonFileDatabase file _ _) = "JSON database: " <+> text file

mkJsonFileDatabase :: (Table t) => FilePath -> JsonFileDatabase t
mkJsonFileDatabase file = JsonFileDatabase file mempty Empty

-- | Default database is stored in @$XDG_DATA_HOME\/imm\/feeds.json@
defaultDatabase :: Table t => IO (JsonFileDatabase t)
defaultDatabase = mkJsonFileDatabase <$> getXdgDirectory XdgData "imm/feeds.json"


data JsonException = UnableDecode
  deriving(Eq, Show)

instance Exception JsonException where
  displayException _ = "Unable to parse JSON"


-- * Interpreter

-- | Interpreter for 'DatabaseF'
mkCoDatabase :: (Table t, FromJSON (Key t), FromJSON (Entry t), ToJSON (Key t), ToJSON (Entry t), MonadIO m, MonadCatch m)
             => JsonFileDatabase t -> CoDatabaseF t m (JsonFileDatabase t)
mkCoDatabase t = CoDatabaseF coDescribe coFetch coFetchAll coUpdate coInsert coDelete coPurge coCommit where
  coDescribe = return (pretty t, t)
  coFetch keys = do
    (cache, t') <- coFetchAll
    let result = fmap (Map.filterWithKey (\uri _ -> member uri $ Set.fromList keys)) cache
    return (result, t')
  coFetchAll = handleAll (\e -> return (Left e, t)) $ do
    t'@(JsonFileDatabase _ cache _) <- loadInCache t
    return (Right cache, t')
  coUpdate key f = exec (\a -> update a key f)
  coInsert rows = exec (`insert` rows)
  coDelete keys = exec (`delete` keys)
  coPurge = exec purge
  coCommit = exec commit
  exec f = handleAll (\e -> return (Left e, t)) $ (Right (),) <$> f t


-- * Low-level implementation

loadInCache :: (Table t, MonadIO m, MonadCatch m, FromJSON (Key t), FromJSON (Entry t))
            => JsonFileDatabase t -> m (JsonFileDatabase t)
loadInCache t@(JsonFileDatabase file _ status) = case status of
  Empty -> do
    io $ createDirectoryIfMissing True $ takeDirectory file
    fileContent <- hGetContents =<< io (openFile file ReadWriteMode)
    cache <- (`failWith` UnableDecode) $ fmap Map.fromList $ decode $ fromEmpty "[]" fileContent
    return $ JsonFileDatabase file cache Clean
  _ -> return t
  where fromEmpty x "" = x
        fromEmpty _ y  = y


insert :: (Table t, MonadIO m, MonadCatch m, FromJSON (Key t), FromJSON (Entry t))
       => JsonFileDatabase t -> [(Key t, Entry t)] -> m (JsonFileDatabase t)
insert t rows = insertInCache rows <$> loadInCache t

insertInCache :: Table t => [(Key t, Entry t)] -> JsonFileDatabase t -> JsonFileDatabase t
insertInCache rows (JsonFileDatabase file cache _) = JsonFileDatabase file (Map.union cache $ Map.fromList rows) Dirty


update :: (Table t, MonadIO m, MonadCatch m, FromJSON (Key t), FromJSON (Entry t))
       => JsonFileDatabase t -> Key t -> (Entry t -> Entry t) -> m (JsonFileDatabase t)
update t key f = updateInCache key f <$> loadInCache t

updateInCache :: Table t => Key t -> (Entry t -> Entry t) -> JsonFileDatabase t -> JsonFileDatabase t
updateInCache key f (JsonFileDatabase file cache _) = JsonFileDatabase file newCache Dirty where
  newCache = Map.update (Just . f) key cache

delete :: (Table t, MonadIO m, MonadCatch m, FromJSON (Key t), FromJSON (Entry t))
       => JsonFileDatabase t -> [Key t] -> m (JsonFileDatabase t)
delete t keys = deleteInCache keys <$> loadInCache t

deleteInCache :: Table t => [Key t] -> JsonFileDatabase t -> JsonFileDatabase t
deleteInCache keys (JsonFileDatabase file cache _) = JsonFileDatabase file newCache Dirty where
  newCache = foldr Map.delete cache keys

purge :: (Table t, MonadIO m, MonadCatch m, FromJSON (Key t), FromJSON (Entry t))
      => JsonFileDatabase t -> m (JsonFileDatabase t)
purge t = purgeInCache <$> loadInCache t

purgeInCache :: Table t => JsonFileDatabase t -> JsonFileDatabase t
purgeInCache (JsonFileDatabase file _ _) = JsonFileDatabase file mempty Dirty

commit :: (MonadIO m, ToJSON (Key t), ToJSON (Entry t))
       => JsonFileDatabase t -> m (JsonFileDatabase t)
commit t@(JsonFileDatabase file cache status) = case status of
  Dirty -> do
    writeFile file $ encode $ Map.toList cache
    return $ JsonFileDatabase file cache Clean
  _ -> return t
