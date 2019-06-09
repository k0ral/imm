{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
-- | Feed table definitions. This is a specialization of "Imm.Database".
module Imm.Database.FeedTable where

-- {{{ Imports
import           Imm.Database           as Database
import           Imm.Feed
import           Imm.Logger             as Logger
import           Imm.Pretty

import           Control.Exception.Safe
import           Control.Monad.Time
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Hashable
import           Data.Map.Lazy          (mapKeys)
import qualified Data.Map.Lazy          as Map (insert, filter, keys)
import qualified Data.Set               as Set (intersection)
import           Data.Time
import           URI.ByteString

-- To be removed soon
import           Text.Atom.Types
import           Text.RSS.Types
-- }}}

-- * Types

-- | Unique key in feeds table
newtype FeedID = FeedID URI
  deriving(Eq, Ord, Show)

prettyFeedID :: FeedID -> Doc AnsiStyle
prettyFeedID (FeedID uri) = prettyURI uri

instance FromJSON FeedID where
  parseJSON value = FeedID . _unwrapURI <$> parseJSON value

instance ToJSON FeedID where
  toJSON (FeedID uri) = toJSON $ JsonURI uri

instance Pretty FeedID where
  pretty (FeedID uri) = prettyURI uri


-- DEPRECATED
getHashes :: FeedElement -> [Int]
getHashes (RssElement item) = map (hash @String . show . prettyGuid) (maybeToList $ itemGuid item)
  <> map (hash @String . show . withRssURI prettyURI) (maybeToList $ itemLink item)
  <> [hash $ itemTitle item]
  <> [hash $ itemDescription item]
getHashes (AtomElement entry) = [hash $ entryId entry, (hash :: String -> Int) $ show $ prettyAtomText $ entryTitle entry]


-- | Newtype wrapper to provide 'FromJSON' and 'ToJSON' instances for 'URI'
newtype JsonURI = JsonURI { _unwrapURI :: URI }

instance FromJSON JsonURI where
  parseJSON = withText "URI" $ \s ->
    either (const $ fail "Invalid URI") (return . JsonURI) $ parseURI laxURIParserOptions $ encodeUtf8 s

instance ToJSON JsonURI where
  toJSON = String . decodeUtf8 . serializeURIRef' . _unwrapURI


-- | Newtype wrapper to provide `FromJSON` and `ToJSON` instances for 'FeedElement'
newtype JsonElement = JsonElement { unwrapElement :: FeedElement } deriving(Eq, Ord)

instance FromJSON JsonElement where
  parseJSON = withText "Feed element" $ \t -> parseFeedElement t & liftEither <&> JsonElement
    where liftEither :: Either e a -> Parser a
          liftEither = either (const mempty) return

instance FromJSONKey JsonElement

instance ToJSON JsonElement where
  toJSON (JsonElement element) = String $ renderFeedElement element

instance ToJSONKey JsonElement


data DatabaseEntry = DatabaseEntry
  { entryURI         :: URI
  , entryTags        :: Set Text
  , entryReadHashes  :: Set Int
  , entryFeed        :: Maybe Feed
  , entryItems       :: Map FeedElement Bool
  , entryLastUpdate  :: Maybe UTCTime
  } deriving(Eq, Show)

prettyShortDatabaseEntry :: DatabaseEntry -> Doc AnsiStyle
prettyShortDatabaseEntry DatabaseEntry{..} = magenta feedID
  <++> indent 3 tags
  <++> indent 3 ("Last update:" <+> lastUpdate)
  <++> indent 3 (yellow (pretty totalItems) <+> "items," <+> yellow (pretty totalUnprocessedItems) <+> "unprocessed")

  where feedID = prettyURI entryURI
        tags = sep $ map ((<>) "#" . pretty) $ toList entryTags
        lastUpdate = maybe "never" prettyTime entryLastUpdate
        totalItems = length entryItems
        totalUnprocessedItems = length $ Map.filter not entryItems

prettyDatabaseEntry :: DatabaseEntry -> Doc AnsiStyle
prettyDatabaseEntry DatabaseEntry{..} = magenta feedID
  <++> tags
  <++> ("Last update:" <+> lastUpdate)
  <++> (yellow (pretty $ length $ Map.filter id entryItems) <+> "processed items")
  <++> indent 3 (vsep $ map displayItem $ Map.keys $ Map.filter id entryItems)
  <++> (yellow (pretty $ length $ Map.filter not entryItems) <+> "unprocessed items")
  <++> indent 3 (vsep $ map displayItem $ Map.keys $ Map.filter not entryItems)
  where feedID = prettyURI entryURI
        tags = sep $ map ((<>) "#" . pretty) $ toList entryTags
        lastUpdate = maybe "never" prettyTime entryLastUpdate
        displayItem item = maybe "<unknown>" prettyTime (getDate item) <+> pretty (getTitle item)

instance FromJSON DatabaseEntry where
  parseJSON (Object v) = DatabaseEntry
    <$> (_unwrapURI <$> (v .: "uri"))
    <*> v .: "tags"
    <*> (v .: "readHashes" <|> pure mempty)
    <*> ((v .:? "feed") >>= maybe (pure Nothing) (fmap Just . liftEither . parseFeed))
    <*> (v .:? "items" >>= maybe (pure mempty) (return . mapKeys unwrapElement))
    <*> (v .: "lastUpdate" <|> v .: "lastCheck")
    where liftEither :: Either e a -> Parser a
          liftEither = either (const mempty) return

  parseJSON _          = mzero

instance ToJSON DatabaseEntry where
  toJSON DatabaseEntry{..} = object $
    [ "uri"        .= JsonURI entryURI
    , "tags"       .= entryTags
    , "items"      .= mapKeys JsonElement entryItems
    , "lastUpdate"  .= entryLastUpdate
    ] <> maybeToList (fmap (("feed" .=) . renderFeed) entryFeed)
      <> if null entryReadHashes then mempty else ["readHashes" .= entryReadHashes]

newDatabaseEntry :: FeedID -> Set Text -> DatabaseEntry
newDatabaseEntry (FeedID uri) tags = DatabaseEntry uri tags mempty mzero mempty Nothing

-- | Singleton type to represent feeds table
data FeedTable = FeedTable deriving(Eq, Ord, Read, Show)

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
  return $ maybe New LastUpdate $ entryLastUpdate =<< result

markAsProcessed :: MonadThrow m => MonadTime m
                => Logger.Handle m -> Database.Handle m FeedTable -> FeedID -> FeedElement -> m ()
markAsProcessed logger database feedID element = do
  log logger Debug $ "Marking as processed:" <+> pretty (PrettyKey element) <> "..."
  utcTime <- currentTime
  update database feedID $ \entry -> entry
    { entryItems = Map.insert element True $ entryItems entry
    , entryLastUpdate = Just utcTime
    }


markAsUnprocessed :: MonadThrow m
                  => Logger.Handle m -> Database.Handle m FeedTable -> FeedID -> m ()
markAsUnprocessed logger database feedID = do
  log logger Debug $ "Marking as unprocessed:" <+> pretty feedID <> "..."
  update database feedID $ \entry -> entry
    { entryItems = False <$ entryItems entry
    , entryLastUpdate = Nothing
    , entryReadHashes = mempty
    }

isRead :: MonadCatch m => Database.Handle m FeedTable -> FeedID -> FeedElement -> m Bool
isRead database feedID element = do
  DatabaseEntry _ _ readHashes _ items lastUpdate <- Database.fetch database feedID
  let matchHash = not $ null $ fromList (getHashes element) `Set.intersection` readHashes
      matchDate = case (lastUpdate, getDate element) of
        (Nothing, _)     -> False
        (_, Nothing)     -> False
        (Just a, Just b) -> a > b
      matchKey = lookup element items & fromMaybe False
  return $ matchHash || matchDate || matchKey
