{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators     #-}
-- | Feed table definitions. This is a specialization of "Imm.Database".
module Imm.Database.FeedTable where

-- {{{ Imports
import Imm.Aeson
import Imm.Database
import           Imm.Logger
import Imm.Prelude
import Imm.Pretty

import Control.Monad.Trans.Free

import           Data.Aeson
import           Data.Set (Set)
import           Data.Time           as Time

import URI.ByteString
-- }}}

-- * Types

-- | Unique key in feeds table
newtype FeedID = FeedID URI
  deriving(Eq, Ord, Show)

instance FromJSON FeedID where
  parseJSON = fmap FeedID . parseJsonURI

instance ToJSON FeedID where
  toJSON (FeedID uri) = toJsonURI uri

instance Pretty FeedID where
  pretty (FeedID uri) = prettyURI uri


data DatabaseEntry = DatabaseEntry
  { entryURI         :: URI
  , entryCategory    :: Text
  , entryReadHashes  :: Set Int
  , entryLastCheck   :: Maybe UTCTime
  } deriving(Eq, Show)

instance Pretty DatabaseEntry where
  pretty r = text "Entry:" <+> prettyURI (entryURI r) <++> indent 2
    ( text "Category:" <+> text (fromText $ entryCategory r)
    <++> text "Last check:" <+> text (maybe "<never>" (formatTime defaultTimeLocale rfc822DateFormat) $ entryLastCheck r)
    <++> text "Read hashes:" <+> text (show $ length $ entryReadHashes r)
    )

instance FromJSON DatabaseEntry where
  parseJSON (Object v) = DatabaseEntry <$> (parseJsonURI =<< v .: "uri") <*> v .: "category" <*> v.: "readHashes" <*> v .: "lastCheck"
  parseJSON _          = mzero

instance ToJSON DatabaseEntry where
  toJSON entry = object
    [ "uri"        .= toJsonURI (entryURI entry)
    , "category"   .= entryCategory entry
    , "readHashes" .= entryReadHashes entry
    , "lastCheck"  .= entryLastCheck entry
    ]

newDatabaseEntry :: FeedID -> Text -> DatabaseEntry
newDatabaseEntry (FeedID uri) category = DatabaseEntry uri category mempty Nothing

-- | Singleton type to represent feeds table
data FeedTable = FeedTable
  deriving(Show)

instance Pretty FeedTable where
  pretty _ = "Feeds table"

instance Table FeedTable where
  type Key FeedTable = FeedID
  type Entry FeedTable = DatabaseEntry


data FeedStatus = Unknown | New | LastUpdate UTCTime

instance Pretty FeedStatus where
  pretty Unknown        = text "Unknown"
  pretty New            = text "New"
  pretty (LastUpdate x) = text "Last update:" <+> text (formatTime defaultTimeLocale rfc822DateFormat x)


data Database = Database [DatabaseEntry]
  deriving (Eq, Show)

type DatabaseF' = DatabaseF FeedTable
type CoDatabaseF' = CoDatabaseF FeedTable

-- * Primitives

register :: (MonadThrow m, LoggerF :<: f, DatabaseF' :<: f, Functor f, MonadFree f m)
          => FeedID -> Text -> m ()
register feedID category = do
  logInfo $ "Registering feed " <> magenta (pretty feedID) <> "..."
  insert FeedTable feedID $ newDatabaseEntry feedID category

getStatus :: (DatabaseF' :<: f, Functor f, MonadFree f m, MonadCatch m)
          => FeedID -> m FeedStatus
getStatus feedID = handleAny (\_ -> return Unknown) $ do
  result <- fmap Just (fetch FeedTable feedID) `catchAny` (\_ -> return Nothing)
  return $ maybe New LastUpdate $ entryLastCheck =<< result

addReadHash :: (DatabaseF' :<: f, Functor f, MonadFree f m, MonadThrow m, LoggerF :<: f)
               => FeedID -> Int -> m ()
addReadHash feedID hash = do
  logDebug $ "Adding read hash: " <> pretty hash <> "..."
  update FeedTable feedID f
  where f a = a { entryReadHashes = insertSet hash $ entryReadHashes a }

-- | Set the last check time to now
markAsRead :: (MonadIO m, DatabaseF' :<: f, Functor f, MonadFree f m, MonadThrow m, LoggerF :<: f)
           => FeedID -> m ()
markAsRead feedID = do
  logDebug $ "Marking feed as read: " <> pretty feedID <> "..."
  currentTime <- io Time.getCurrentTime
  update FeedTable feedID (f currentTime)
  where f time a = a { entryLastCheck = Just time }

-- | Unset feed's last update and remove all read hashes
markAsUnread ::  (DatabaseF' :<: f, Functor f, MonadFree f m, MonadThrow m, LoggerF :<: f)
             => FeedID -> m ()
markAsUnread feedID = do
  logInfo $ "Marking feed as unread: " <> show (pretty feedID) <> "..."
  update FeedTable feedID $ \a -> a { entryReadHashes = mempty, entryLastCheck = Nothing }
