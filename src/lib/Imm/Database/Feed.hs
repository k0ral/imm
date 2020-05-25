{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
-- | Feed table definitions.
module Imm.Database.Feed where

-- {{{ Imports
import           Imm.Feed
import           Imm.Logger             (log, LogLevel(..))
import qualified           Imm.Logger             as Logger
import           Imm.Pretty

import           Control.Exception.Safe hiding(handle)
import           Control.Monad.Time
import           Data.Aeson.Extended
import           Data.Hashable
import           Data.Map          (mapKeys)
import qualified Data.Map          as Map
import qualified Data.Set               as Set (intersection)
import           Data.Time

-- To be removed soon
import           Text.Atom.Types
import           Text.RSS.Types
-- }}}

-- * Types

-- DEPRECATED
getHashes :: FeedElement -> [Int]
getHashes (RssElement item) = map (hash @String . show . prettyGuid) (maybeToList $ itemGuid item)
  <> map (hash @String . show . withRssURI prettyURI) (maybeToList $ itemLink item)
  <> [hash $ itemTitle item]
  <> [hash $ itemDescription item]
getHashes (AtomElement entry) = [hash $ entryId entry, (hash :: String -> Int) $ show $ prettyAtomText $ entryTitle entry]


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


data Entry = Entry
  { entryLocation    :: FeedLocation
  , entryTags        :: Set Text
  , entryReadHashes  :: Set Int
  , entryFeed        :: Maybe Feed
  , entryItems       :: Map FeedElement Bool
  , entryLastUpdate  :: Maybe UTCTime
  }
  deriving(Eq, Ord, Show)

prettyShortEntry :: Entry -> Doc AnsiStyle
prettyShortEntry Entry{..} = magenta feedID
  <++> indent 3 tags
  <++> indent 3 ("Last update:" <+> lastUpdate)
  <++> indent 3 (yellow (pretty totalItems) <+> "items," <+> yellow (pretty totalUnprocessedItems) <+> "unprocessed")

  where feedID = pretty entryLocation
        tags = sep $ map ((<>) "#" . pretty) $ toList entryTags
        lastUpdate = maybe "never" prettyTime entryLastUpdate
        totalItems = length entryItems
        totalUnprocessedItems = length $ Map.filter not entryItems

prettyEntry :: Entry -> Doc AnsiStyle
prettyEntry Entry{..} = magenta (pretty entryLocation)
  <++> tags
  <++> ("Last update:" <+> lastUpdate)
  <++> (yellow (pretty $ length $ Map.filter id entryItems) <+> "processed items")
  <++> indent 3 (vsep $ map displayItem $ Map.keys $ Map.filter id entryItems)
  <++> (yellow (pretty $ length $ Map.filter not entryItems) <+> "unprocessed items")
  <++> indent 3 (vsep $ map displayItem $ Map.keys $ Map.filter not entryItems)
  where tags = sep $ map ((<>) "#" . pretty) $ toList entryTags
        lastUpdate = maybe "never" prettyTime entryLastUpdate
        displayItem item = maybe "<unknown>" prettyTime (getDate item) <+> pretty (getTitle item)

instance FromJSON Entry where
  parseJSON (Object v) = Entry
    <$> ((parseJSON =<< v .: "uri") <|> (parseJSON =<< v .: "location"))
    <*> v .: "tags"
    <*> (v .: "readHashes" <|> pure mempty)
    <*> ((v .:? "feed") >>= maybe (pure Nothing) (fmap Just . liftEither . parseFeed))
    <*> (v .:? "items" >>= maybe (pure mempty) (return . mapKeys unwrapElement))
    <*> (v .: "lastUpdate" <|> v .: "lastCheck")
    where liftEither :: Either e a -> Parser a
          liftEither = either (const mempty) return

  parseJSON _          = mzero

instance ToJSON Entry where
  toJSON Entry{..} = object $
    [ "location"   .= toJSON entryLocation
    , "tags"       .= entryTags
    , "items"      .= mapKeys JsonElement entryItems
    , "lastUpdate"  .= entryLastUpdate
    ] <> maybeToList (fmap (("feed" .=) . renderFeed) entryFeed)
      <> if null entryReadHashes then mempty else ["readHashes" .= entryReadHashes]

makeEntry :: FeedLocation -> Set Text -> Entry
makeEntry feedLocation tags = Entry feedLocation tags mempty mzero mempty Nothing

matching :: FeedQuery -> Int -> Entry -> Bool
matching AllFeeds _ _ = True
matching (ByURI uri) _ entry = case entryLocation entry of
  FeedDirectURI uri' -> uri == uri'
  FeedAlternateLink uri' _ -> uri == uri'
matching (ByDatabaseID i) j _ = i == j


data FeedStatus = Unknown | New | LastUpdate UTCTime

instance Pretty FeedStatus where
  pretty Unknown        = "Unknown"
  pretty New            = "New"
  pretty (LastUpdate x) = "Last update:" <+> pretty (formatTime defaultTimeLocale rfc822DateFormat x)



data EntryKey = ByLocation FeedLocation | ById Int
  deriving(Eq, Ord, Show)

instance Pretty EntryKey where
  pretty (ByLocation location) = pretty location
  pretty (ById i) = "ID" <+> pretty i


data Handle m = Handle
  { _describeDatabase :: m (Doc AnsiStyle)
  , _fetch  :: [EntryKey] -> m (Map EntryKey (Int, Entry))
  , _fetchAll         :: m (Map Int Entry)
  , _update :: [EntryKey] -> (Entry -> Entry) -> m ()
  , _insert           :: [Entry ] -> m (Map FeedLocation Int)
  , _delete      :: [EntryKey] -> m ()
  , _purge            :: m ()
  , _commit           :: m ()
  }

readOnly :: Monad m => Logger.Handle m -> Handle m -> Handle m
readOnly logger handle = handle
  { _describeDatabase = do
      output <- _describeDatabase handle
      return $ output <+> yellow (brackets "read only")
  , _update = \keys _ -> log logger Debug $ "Not updating database for keys" <+> pretty keys
  , _insert = \entries -> log logger Debug ("Not inserting " <> yellow (pretty $ length entries) <> " entries") $> mempty
  , _delete = \keys -> log logger Debug $ "Not deleting " <> yellow (pretty $ length keys) <> " entries"
  , _purge = log logger Debug "Not purging database"
  , _commit = log logger Debug "Not committing database"
  }



data DatabaseException
  = NotCommitted
  | NotDeleted [FeedLocation]
  | KeyNotFound [EntryKey]
  | NotInserted [Entry]
  | NotPurged
  | NotUpdated FeedLocation
  | UnableFetchAll
  deriving(Eq, Show)

instance Exception DatabaseException where
  displayException = show . pretty

instance Pretty DatabaseException where
  pretty NotCommitted = "Unable to commit database changes."
  pretty (NotDeleted x) = "Unable to delete the following entries in database:" <++> indent 2 (vsep $ map pretty x)
  pretty (KeyNotFound x) = "Unable to find the following entries in database:" <++> indent 2 (vsep $ map pretty x)
  pretty (NotInserted x) = "Unable to insert the following entries in database:" <++> indent 2 (vsep $ map (pretty . entryLocation) x)
  pretty NotPurged = "Unable to purge feed database"
  pretty (NotUpdated x) = "Unable to update the following entry in database:" <++> indent 2 (pretty x)
  pretty UnableFetchAll = "Unable to fetch all entries from database."


-- * Low-level primitives

fetch1 :: Monad m => MonadThrow m => Handle m -> EntryKey -> m (Int, Entry)
fetch1 handle k = do
  results <- _fetch handle [k]
  maybe (throwM $ KeyNotFound [k]) return $ lookup k results


fetch :: Monad m => Handle m -> [EntryKey] -> m (Map EntryKey (Int, Entry))
fetch = _fetch

fetchQuery :: Monad m => Handle m -> (Int -> Entry -> Bool) -> m (Map Int Entry)
fetchQuery handle f = _fetchAll handle
  <&> Map.filterWithKey f

fetchAll :: Monad m => Handle m -> m (Map Int Entry)
fetchAll = _fetchAll

update :: Monad m => Handle m -> [EntryKey] -> (Entry -> Entry) -> m ()
update = _update

insert :: MonadThrow m => Logger.Handle m -> Handle m -> Entry -> m Int
insert logger handle entry = insertList logger handle [entry]
  <&> Map.lookup (entryLocation entry)
  >>= maybe (throwM $ NotInserted [entry]) pure

insertList :: Monad m => Logger.Handle m -> Handle m -> [Entry] -> m (Map FeedLocation Int)
insertList logger handle i = do
  log logger Info $ "Inserting " <> yellow (pretty $ length i) <> " entries..."
  _insert handle i

delete1 :: Monad m => Logger.Handle m -> Handle m -> EntryKey -> m ()
delete1 logger handle k = delete logger handle [k]

delete :: Monad m => Logger.Handle m -> Handle m -> [EntryKey] -> m ()
delete logger handle k = do
  log logger Info $ "Deleting " <> yellow (pretty $ length k) <> " entries..."
  _delete handle k

purge :: Monad m => Logger.Handle m -> Handle m -> m ()
purge logger handle = do
  log logger Info "Purging database..."
  _purge handle

commit :: Monad m => Logger.Handle m -> Handle m -> m ()
commit logger handle = do
  log logger Debug "Committing database transaction..."
  _commit handle
  log logger Debug "Database transaction committed"


-- * High-level queries

register :: MonadThrow m => Logger.Handle m -> Handle m -> FeedLocation -> Set Text -> m Int
register logger database feedLocation tags = do
  log logger Info $ "Registering feed" <+> magenta (pretty feedLocation) <> "..."
  insert logger database $ makeEntry feedLocation tags

getStatus :: MonadCatch m => Handle m -> EntryKey -> m FeedStatus
getStatus database key = handleAny (\_ -> return Unknown) $ do
  result <- fmap Just (fetch1 database key) `catchAny` (\_ -> return Nothing)
  return $ maybe New LastUpdate $ entryLastUpdate . snd =<< result

markAsProcessed :: MonadThrow m => MonadTime m
                => Logger.Handle m -> Handle m -> EntryKey -> FeedElement -> m ()
markAsProcessed logger database key element = do
  log logger Debug $ "Marking as processed:" <+> pretty (PrettyKey element) <> "..."
  utcTime <- currentTime
  update database [key] $ \entry -> entry
    { entryItems = Map.insert element True $ entryItems entry
    , entryLastUpdate = Just utcTime
    }


markAsUnprocessed :: MonadThrow m => Logger.Handle m -> Handle m -> EntryKey -> m ()
markAsUnprocessed logger database key = do
  log logger Debug $ "Marking as unprocessed:" <+> pretty key <> "..."
  update database [key] $ \entry -> entry
    { entryItems = False <$ entryItems entry
    , entryLastUpdate = Nothing
    , entryReadHashes = mempty
    }

listUnprocessedElements :: MonadThrow m => Handle m -> EntryKey -> m [FeedElement]
listUnprocessedElements database key = fetch1 database key
  <&> snd
  <&> entryItems
  <&> Map.filterWithKey (\_ v -> not v)
  <&> Map.keys

isRead :: MonadCatch m => Handle m -> EntryKey -> FeedElement -> m Bool
isRead database key element = do
  (_, entry) <- fetch1 database key
  let matchHash = not $ null $ fromList (getHashes element) `Set.intersection` entryReadHashes entry
      matchDate = case (entryLastUpdate entry, getDate element) of
        (Nothing, _)     -> False
        (_, Nothing)     -> False
        (Just a, Just b) -> a > b
      matchKey = Map.findWithDefault False element $ entryItems entry
  return $ matchHash || matchDate || matchKey

resolveEntryKey :: Monad m => Handle m -> FeedQuery -> m [EntryKey]
resolveEntryKey _ (ByDatabaseID i) = pure [ById i]
resolveEntryKey database (ByURI uri) = fetchAll database
  <&> Map.filter f
  <&> toList
  <&> map (ByLocation . entryLocation)
  where f entry = case entryLocation entry of
          FeedDirectURI uri' -> uri == uri'
          FeedAlternateLink uri' _ -> uri == uri'
resolveEntryKey database AllFeeds = fetchAll database
  <&> toList
  <&> map (ByLocation . entryLocation)

resolveFeedLocation :: Monad m => Handle m -> EntryKey -> m FeedLocation
resolveFeedLocation database (ById i) = fetchAll database
  <&> Map.elemAt i
  <&> snd
  <&> entryLocation
resolveFeedLocation _ (ByLocation location) = return location
