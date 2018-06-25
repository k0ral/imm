{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
-- | Feed table definitions. This is a specialization of "Imm.Database".
module Imm.Database.FeedTable where

-- {{{ Imports
import           Imm.Aeson
import           Imm.Database
import           Imm.Logger
import           Imm.Prelude
import           Imm.Pretty

import           Control.Monad.Time
import           Data.Aeson
import           Data.Set           (Set)
import           Data.Time
import           URI.ByteString
-- }}}

-- * Types

-- | Unique key in feeds table
newtype FeedID = FeedID URI
  deriving(Eq, Ord, Show)

prettyFeedID :: FeedID -> Doc AnsiStyle
prettyFeedID (FeedID uri) = prettyURI uri

instance FromJSON FeedID where
  parseJSON = fmap FeedID . parseJsonURI

instance ToJSON FeedID where
  toJSON (FeedID uri) = toJsonURI uri

instance Pretty FeedID where
  pretty (FeedID uri) = prettyURI uri


data DatabaseEntry = DatabaseEntry
  { entryURI        :: URI
  , entryTags       :: Set Text
  , entryReadHashes :: Set Int
  , entryLastCheck  :: Maybe UTCTime
  } deriving(Eq, Show)

prettyDatabaseEntry :: DatabaseEntry -> Doc AnsiStyle
prettyDatabaseEntry entry = magenta feedID
  <++> indent 3 tags
  <++> indent 3 ("Last checked:" <+> lastCheck)

  where feedID = prettyURI $ entryURI entry
        tags = sep $ map ((<>) "#" . pretty) $ toList $ entryTags entry
        lastCheck = format $ entryLastCheck entry
        format = maybe "never" (fromString . formatTime defaultTimeLocale "%F %R")

instance FromJSON DatabaseEntry where
  parseJSON (Object v) = DatabaseEntry <$> (parseJsonURI =<< v .: "uri") <*> v .: "tags" <*> v.: "readHashes" <*> v .: "lastCheck"
  parseJSON _          = mzero

instance ToJSON DatabaseEntry where
  toJSON entry = object
    [ "uri"        .= toJsonURI (entryURI entry)
    , "tags"       .= entryTags entry
    , "readHashes" .= entryReadHashes entry
    , "lastCheck"  .= entryLastCheck entry
    ]

newDatabaseEntry :: FeedID -> Set Text -> DatabaseEntry
newDatabaseEntry (FeedID uri) tags = DatabaseEntry uri tags mempty Nothing

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
  pretty Unknown        = "Unknown"
  pretty New            = "New"
  pretty (LastUpdate x) = "Last update:" <+> pretty (formatTime defaultTimeLocale rfc822DateFormat x)


newtype Database = Database [DatabaseEntry]
  deriving (Eq, Show)

-- * Primitives

register :: (MonadThrow m, MonadLog m, MonadDatabase FeedTable m)
          => FeedID -> Set Text -> m ()
register feedID tags = do
  logInfo $ "Registering feed" <+> magenta (pretty feedID) <> "..."
  insert FeedTable feedID $ newDatabaseEntry feedID tags

getStatus :: (MonadDatabase FeedTable m, MonadCatch m)
          => FeedID -> m FeedStatus
getStatus feedID = handleAny (\_ -> return Unknown) $ do
  result <- fmap Just (fetch FeedTable feedID) `catchAny` (\_ -> return Nothing)
  return $ maybe New LastUpdate $ entryLastCheck =<< result

addReadHash :: (MonadDatabase FeedTable m, MonadThrow m, MonadLog m)
               => FeedID -> Int -> m ()
addReadHash feedID hash = do
  logDebug $ "Adding read hash:" <+> pretty hash <> "..."
  update FeedTable feedID f
  where f a = a { entryReadHashes = insertSet hash $ entryReadHashes a }

-- | Set the last check time to now
markAsRead :: (MonadTime m, MonadDatabase FeedTable m, MonadThrow m, MonadLog m)
           => FeedID -> m ()
markAsRead feedID = do
  logDebug $ "Marking feed as read:" <+> pretty feedID <> "..."
  utcTime <- currentTime
  update FeedTable feedID (f utcTime)
  where f time a = a { entryLastCheck = Just time }

-- | Unset feed's last update and remove all read hashes
markAsUnread :: (MonadDatabase FeedTable m, MonadThrow m, MonadLog m)
             => FeedID -> m ()
markAsUnread feedID = do
  logInfo $ "Marking feed as unread:" <+> prettyFeedID feedID <> "..."
  update FeedTable feedID $ \a -> a { entryReadHashes = mempty, entryLastCheck = Nothing }
