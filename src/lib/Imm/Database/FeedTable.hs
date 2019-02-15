{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
-- | Feed table definitions. This is a specialization of "Imm.Database".
module Imm.Database.FeedTable where

-- {{{ Imports
import           Imm.Aeson
import           Imm.Database           as Database
import           Imm.Logger             as Logger
import           Imm.Pretty

import           Control.Exception.Safe
import           Control.Monad.Time
import           Data.Aeson
import qualified Data.Set               as Set (insert)
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
  rep = FeedTable


data FeedStatus = Unknown | New | LastUpdate UTCTime

instance Pretty FeedStatus where
  pretty Unknown        = "Unknown"
  pretty New            = "New"
  pretty (LastUpdate x) = "Last update:" <+> pretty (formatTime defaultTimeLocale rfc822DateFormat x)


newtype Database = Database [DatabaseEntry]
  deriving (Eq, Show)

-- * Primitives

register :: MonadThrow m => Logger.Handle m -> Database.Handle m FeedTable -> FeedID -> Set Text -> m ()
register logger database feedID tags = do
  log logger Info $ "Registering feed" <+> magenta (pretty feedID) <> "..."
  insert logger database feedID $ newDatabaseEntry feedID tags

getStatus :: MonadCatch m => Database.Handle m FeedTable -> FeedID -> m FeedStatus
getStatus database feedID = handleAny (\_ -> return Unknown) $ do
  result <- fmap Just (fetch database feedID) `catchAny` (\_ -> return Nothing)
  return $ maybe New LastUpdate $ entryLastCheck =<< result

addReadHash :: MonadThrow m => Logger.Handle m -> Database.Handle m FeedTable -> FeedID -> Int -> m ()
addReadHash logger database feedID hash = do
  log logger Debug $ "Adding read hash:" <+> pretty hash <> "..."
  update database feedID f
  where f a = a { entryReadHashes = Set.insert hash $ entryReadHashes a }

-- | Set the last check time to now
markAsRead :: (MonadTime m, MonadThrow m)
           => Logger.Handle m -> Database.Handle m FeedTable -> FeedID -> m ()
markAsRead logger database feedID = do
  log logger Debug $ "Marking feed as read:" <+> pretty feedID <> "..."
  utcTime <- currentTime
  update database feedID (f utcTime)
  where f time a = a { entryLastCheck = Just time }

-- | Unset feed's last update and remove all read hashes
markAsUnread :: MonadThrow m => Logger.Handle m -> Database.Handle m FeedTable -> FeedID -> m ()
markAsUnread logger database feedID = do
  log logger Info $ "Marking feed as unread:" <+> prettyFeedID feedID <> "..."
  update database feedID $ \a -> a { entryReadHashes = mempty, entryLastCheck = Nothing }
