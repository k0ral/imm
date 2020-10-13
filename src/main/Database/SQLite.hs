{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Database.SQLite
  ( DatabaseSQLite(..)
  , defaultDatabase
  , mkHandle
  ) where

import           Database.Handle                          hiding (deleteFeed, insertFeed, insertItem, purge)

import           Data.Aeson
import           Database.Beam
import           Database.Beam.Backend.SQL                (BeamSqlBackend, HasSqlValueSyntax (..))
import           Database.Beam.Backend.SQL.BeamExtensions
-- import           Database.Beam.Migrate
import           Database.Beam.Sqlite                     (runBeamSqlite)
import           Database.SQLite.Simple
import           Imm.Feed
import           Imm.Pretty
import           System.Directory

-- * Schema

data FeedLocationT f = FeedLocationT
  { _locationID    :: Columnar f Int32
  , _locationValue :: Columnar f FeedLocation
  } deriving(Generic, Beamable)

instance Table FeedLocationT where
  data PrimaryKey FeedLocationT f = FeedLocationKey (Columnar f Int32)
    deriving(Generic, Beamable)
  primaryKey = FeedLocationKey . _locationID


data FeedT f = Feed
  { _feedKeyT        :: PrimaryKey FeedLocationT f
  , _feedDefinitionT :: Columnar f FeedDefinition
  , _feedStatusT     :: Columnar f FeedStatus
  } deriving(Generic, Beamable)

instance Table FeedT where
  data PrimaryKey FeedT f = FeedKey (PrimaryKey FeedLocationT f)
    deriving(Generic, Beamable)
  primaryKey = FeedKey . _feedKeyT


data FeedItemT f = FeedItemT
  { _itemKeyT        :: Columnar f Int32
  , _itemFeedKeyT    :: PrimaryKey FeedLocationT f
  , _itemDefinitionT :: Columnar f FeedItem
  , _itemStatusT     :: Columnar f FeedItemStatus
  } deriving(Generic, Beamable)

instance Table FeedItemT where
  data PrimaryKey FeedItemT f = FeedItemKey (Columnar f Int32)
    deriving(Generic, Beamable)
  primaryKey = FeedItemKey . _itemKeyT


data FeedDatabase f = FeedDatabase
  { _feedLocations :: f (TableEntity FeedLocationT)
  , _feeds         :: f (TableEntity FeedT)
  , _feedItems     :: f (TableEntity FeedItemT)
  } deriving (Generic, Database be)


feedDatabase :: DatabaseSettings be FeedDatabase
feedDatabase = defaultDbSettings `withDbModification`
  dbModification
    { _feedItems = modifyTableFields tableModification
        { _itemKeyT = fieldNamed "key"
        , _itemFeedKeyT = FeedLocationKey $ fieldNamed "feed_key"
        , _itemDefinitionT = fieldNamed "definition"
        , _itemStatusT = fieldNamed "status"
        }
    , _feeds = modifyTableFields tableModification
        { _feedKeyT = FeedLocationKey $ fieldNamed "key"
        , _feedDefinitionT = fieldNamed "definition"
        , _feedStatusT = fieldNamed "status"
        }
    , _feedLocations = modifyTableFields tableModification
        { _locationID = fieldNamed "key"
        , _locationValue = fieldNamed "value"
        }
    }


feedLocationsTable :: DatabaseEntity be FeedDatabase (TableEntity FeedLocationT)
feedLocationsTable = _feedLocations feedDatabase

feedsTable :: DatabaseEntity be FeedDatabase (TableEntity FeedT)
feedsTable = _feeds feedDatabase

feedItemsTable :: DatabaseEntity be FeedDatabase (TableEntity FeedItemT)
feedItemsTable = _feedItems feedDatabase

-- * Queries

fetchAllFeeds :: _ [FeedRecord Inserted]
fetchAllFeeds = fmap (map asFeedRecord) $ runSelectReturningList $ select $ do
  feed <- feedsTable & all_
  location <- feedLocationsTable & all_

  guard_ $ _feedKeyT feed `references_` location

  pure (location, feed)

asFeedRecord :: f ~ Identity => (FeedLocationT f, FeedT f) -> FeedRecord Inserted
asFeedRecord (locationT, feed) = FeedRecord
  (fromIntegral $ _locationID locationT)
  (_locationValue locationT)
  (_feedDefinitionT feed)
  (_feedStatusT feed)

fetchFeed :: UID -> _
fetchFeed uid = select (queryFeed uid) & runSelectReturningOne
  >>= maybe (fail $ "Feed not found: " <> show uid) return
  <&> asFeedRecord

queryFeed :: UID -> _
queryFeed uid = do
  feed <- feedsTable & all_
  location <- feedLocationsTable & all_

  guard_ $ _feedKeyT feed `references_` location
  guard_ $ _locationID location ==. val_ (fromIntegral uid)

  pure (location, feed)

fetchItem :: UID -> _
fetchItem uid = feedItemsTable
  & all_
  & filter_ (\i -> primaryKey i ==. val_ (FeedItemKey $ fromIntegral uid))
  & select
  & runSelectReturningOne
  >>= maybe (fail $ "Item not found: " <> show uid) return
  <&> asFeedItemRecord

fetchAllItems :: _ [FeedItemRecord Inserted]
fetchAllItems = select (feedItemsTable & all_)
  & runSelectReturningList
  <&> map asFeedItemRecord

fetchItems :: UID -> _
fetchItems uid = select (queryItems uid) & runSelectReturningList <&> map asFeedItemRecord

queryItems :: UID -> _
queryItems uid = do
  item <- feedItemsTable & all_
  guard_ $ _itemFeedKeyT item ==. val_ (FeedLocationKey $ fromIntegral uid)
  pure item


asFeedItemRecord :: FeedItemT Identity -> FeedItemRecord Inserted
asFeedItemRecord item = FeedItemRecord
  (fromIntegral $ _itemKeyT item)
  (fromIntegral feedKey)
  (_itemDefinitionT item)
  (_itemStatusT item)
  where FeedLocationKey feedKey = _itemFeedKeyT item


deleteFeed :: UID -> _ ()
deleteFeed uid = do
  -- runDelete $ delete feedItemsTable $ \item -> _itemFeedKeyT item ==. val_ (FeedLocationKey $ fromIntegral uid)
  -- runDelete $ delete feedsTable $ \feed -> _feedKeyT feed ==. val_ (FeedLocationKey $ fromIntegral uid)
  runDelete $ delete feedLocationsTable $ \location -> _locationID location ==. val_ (fromIntegral uid)

insertFeed :: FeedRecord NotInserted -> _ (FeedRecord Inserted)
insertFeed record = do
  location <- insertFeedLocation $ _feedLocation record
  let value = Feed (primaryKey location) (_feedDefinition record) (_feedStatus record)
  feed <- insertValues [value]
    & insert feedsTable
    & runInsertReturningList
    >>= headFail (displayException $ FeedsNotInserted [record])
  return $ asFeedRecord (location, feed)

insertFeedLocation :: FeedLocation -> _
insertFeedLocation location = insertExpressions [FeedLocationT default_ (val_ location)]
  & insert feedLocationsTable
  & runInsertReturningList
  >>= headFail ("Unable to insert feed location " <> show location)

insertItem :: FeedItemRecord NotInserted -> _
insertItem record = do
  inserted <- insertExpressions [FeedItemT default_ (val_ $ FeedLocationKey $ fromIntegral $ _itemFeedKey record) (val_ $ _itemDefinition record) (val_ $ _itemStatus record)]
    & insert feedItemsTable
    & runInsertReturningList
    >>= headFail (displayException $ ItemsNotInserted [record])
  return $ FeedItemRecord (fromIntegral $ _itemKeyT inserted) (_itemFeedKey record) (_itemDefinition record) (_itemStatus record)

purge = runDelete $ delete feedLocationsTable $ const $ val_ True

updateItemStatus :: FeedItemRecord Inserted -> _
updateItemStatus record = runUpdate $ update feedItemsTable
  (\item -> _itemStatusT item <-. val_ (_itemStatus record))
  (\item -> primaryKey item ==. val_ (FeedItemKey $ fromIntegral $ _itemKey record))

updateFeedDefinition :: FeedRecord Inserted -> _
updateFeedDefinition record = runUpdate $ update feedsTable
  (\feed -> _feedDefinitionT feed <-. val_ (_feedDefinition record))
  (\feed -> primaryKey feed ==. val_ (FeedKey $ FeedLocationKey $ fromIntegral $ _feedKey record))

updateFeedStatus :: FeedRecord Inserted -> _
updateFeedStatus record = runUpdate $ update feedsTable
  (\feed -> _feedStatusT feed <-. val_ (_feedStatus record))
  (\feed -> primaryKey feed ==. val_ (FeedKey $ FeedLocationKey $ fromIntegral $ _feedKey record))

-- * Handle

newtype DatabaseSQLite = DatabaseSQLite
 { _sqliteFile           :: FilePath
 }

instance Pretty DatabaseSQLite where
 pretty db = "SQLite database: " <+> pretty (_sqliteFile db)

-- | Default database is stored in @$XDG_CONFIG_HOME\/imm\/database.sqlite@
defaultDatabase :: IO DatabaseSQLite
defaultDatabase = DatabaseSQLite <$> getXdgDirectory XdgConfig "imm/database.sqlite"


createTables :: Connection -> IO ()
createTables conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS locations (key INTEGER NOT NULL PRIMARY KEY, value BLOB NOT NULL UNIQUE);"
  execute_ conn "CREATE TABLE IF NOT EXISTS feeds ( \
    \ key INTEGER NOT NULL PRIMARY KEY, definition BLOB NOT NULL, status BLOB NOT NULL, \
    \ CONSTRAINT fk_feeds FOREIGN KEY (key) REFERENCES locations(key) ON DELETE CASCADE \
    \ );"
  execute_ conn "CREATE TABLE IF NOT EXISTS items ( \
    \ key INTEGER NOT NULL PRIMARY KEY, feed_key INTEGER NOT NULL, definition BLOB NOT NULL, status BLOB NOT NULL, \
    \ CONSTRAINT fk_feeds FOREIGN KEY (feed_key) REFERENCES locations(key) ON DELETE CASCADE \
    \ );"

mkHandle :: DatabaseSQLite -> IO (Handle IO)
mkHandle database = do
  conn <- open $ _sqliteFile database
  createTables conn
  execute_ conn "PRAGMA foreign_keys = ON;"

  return $ Handle
    { _describeDatabase = return $ pretty database
    , _fetchAllFeeds = runBeamSqlite conn fetchAllFeeds
    , _fetchFeed = runBeamSqlite conn . fetchFeed
    , _fetchItems = runBeamSqlite conn . fetchItems
    , _fetchAllItems = runBeamSqlite conn fetchAllItems
    , _fetchItem = runBeamSqlite conn . fetchItem
    , _updateFeedDefinition = runBeamSqlite conn . updateFeedDefinition
    , _updateFeedStatus = runBeamSqlite conn . updateFeedStatus
    , _updateItemStatus = runBeamSqlite conn . updateItemStatus
    , _insertFeed = runBeamSqlite conn . insertFeed
    , _insertItem = runBeamSqlite conn . insertItem
    , _deleteFeed = runBeamSqlite conn . deleteFeed
    , _purge = runBeamSqlite conn purge
    , _commit = return ()
    }

instance (BeamSqlBackend be, FromBackendRow be LByteString) => FromBackendRow be FeedLocation where
  fromBackendRow = fromBackendRow <&> decode >>= maybe empty pure

instance HasSqlValueSyntax be ByteString => HasSqlValueSyntax be FeedLocation where
  sqlValueSyntax = sqlValueSyntax . toStrict . encode

instance (BeamSqlBackend be, FromBackendRow be LByteString) => FromBackendRow be FeedItem where
  fromBackendRow = fromBackendRow <&> decode >>= maybe empty pure

instance HasSqlValueSyntax be ByteString => HasSqlValueSyntax be FeedItem where
  sqlValueSyntax = sqlValueSyntax . toStrict . encode

instance (BeamSqlBackend be, FromBackendRow be LByteString) => FromBackendRow be FeedDefinition where
  fromBackendRow = fromBackendRow <&> decode >>= maybe empty pure

instance HasSqlValueSyntax be ByteString => HasSqlValueSyntax be FeedDefinition where
  sqlValueSyntax = sqlValueSyntax . toStrict . encode

instance (BeamSqlBackend be, FromBackendRow be LByteString) => FromBackendRow be FeedStatus where
  fromBackendRow = fromBackendRow <&> decode >>= maybe empty pure

instance HasSqlValueSyntax be ByteString => HasSqlValueSyntax be FeedStatus where
  sqlValueSyntax = sqlValueSyntax . toStrict . encode

instance (BeamSqlBackend be, FromBackendRow be LByteString) => FromBackendRow be FeedItemStatus where
  fromBackendRow = fromBackendRow <&> decode >>= maybe empty pure

instance HasSqlValueSyntax be ByteString => HasSqlValueSyntax be FeedItemStatus where
  sqlValueSyntax = sqlValueSyntax . toStrict . encode
