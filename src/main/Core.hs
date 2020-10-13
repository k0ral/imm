{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
module Core (
  markAsUnprocessed,
  subscribe,
  unsubscribe,
  describeFeeds,
  describeFeed,
  downloadFeed,
) where

-- {{{ Imports
import           Alternate
import           Database.Record
-- (FeedItemRecord (..), FeedItemStatus (..), FeedRecord (..), Inserted)
import qualified Database.Handle           as Database
import           Output                    (putDocLn)
import qualified Output

import           Control.Exception.Safe
import qualified Data.Text                 as Text
import           Imm.Feed
import           Imm.HTTP                  as HTTP
import           Imm.Link
import           Imm.Logger                as Logger
import           Imm.Pretty
import           Network.HTTP.Types.Header
import           Pipes
import           Safe
import           Text.XML                  as XML ()
import           URI.ByteString.Extended
-- }}}


markAsUnprocessed :: MonadThrow m => MonadIO m
                 => Logger.Handle m
                 -> Database.Handle m
                 -> FeedQuery
                 -> m ()
markAsUnprocessed logger database QueryAll         = Database.markAllFeedsAsUnprocessed logger database
markAsUnprocessed logger database (QueryByUID uid) = Database.markFeedAsUnprocessed logger database uid

-- | Print database status for given feed(s)
describeFeeds :: MonadThrow m => MonadIO m
  => Output.Handle m -> Database.Handle m -> FeedQuery -> m ()
describeFeeds output database QueryAll = do
  feeds <- Database._fetchAllFeeds database
  when (null feeds) $ putDocLn output "No subscriptions"
  forM_ feeds $ describeFeed output database
describeFeeds output database (QueryByUID uid) = do
  feed <- Database._fetchFeed database uid
  describeFeed output database feed


describeFeed :: MonadThrow m => MonadIO m
  => Output.Handle m -> Database.Handle m -> FeedRecord Inserted -> m ()
describeFeed output database FeedRecord{..} = do
  items <- Database._fetchItems database _feedKey

  let newItems = length $ filter (not . _isProcessed . _itemStatus) items
      description = pretty _feedDefinition
        <++> pretty _feedStatus
        <++> yellow (pretty newItems) <> "/" <> pretty (length items) <+> "new items"

  putDocLn output $ pretty _feedKey <+> magenta (pretty _feedLocation)
    <++> indent 3 description


-- | Register the given set of feeds in database
subscribe :: MonadCatch m => MonadIO m
          => Logger.Handle m -> Output.Handle m -> Database.Handle m
          -> FeedLocation -> Set Text -> m ()
subscribe logger output database feedLocation tags = do
  feedRecord <- Database.register logger database feedLocation tags
  putDocLn output $ "Subscribed with index" <+> prettyKey feedRecord

-- | Un-register the given set of feeds from database
unsubscribe :: MonadThrow m
            => Logger.Handle m
            -> Database.Handle m
            -> FeedQuery
            -> m ()
unsubscribe logger database QueryAll         = Database.purge logger database
unsubscribe logger database (QueryByUID uid) = Database.deleteFeed logger database uid

-- | Download feed data, following alternate links if necessary
downloadFeed :: m ~ IO => Logger.Handle m -> HTTP.Handle m -> (Producer ByteString m () -> m a) -> FeedLocation -> m a
downloadFeed logger httpClient f (FeedLocation uri title) = HTTP.withGet logger httpClient uri $ \response -> do
  case getHeader hContentType response >>= parseMediaType of
    Just MediaTypeRSS{}  -> f (responseBody response)
    Just MediaTypeAtom{} -> f (responseBody response)
    Just MediaTypeApplicationXML{} -> f (responseBody response)
    Just MediaTypeTextXML{} -> f (responseBody response)
    Just MediaTypeHTML{} -> extractAlternateLinks logger uri (responseBody response)
      <&> filterByTitle
      <&> maximumByMay compareFeeds
      >>= maybe (throwM $ FeedNotFound uri) (\l -> downloadFeed logger httpClient f $ FeedLocation (toAbsoluteURI (Scheme "https") $ _linkURI l) mempty)
    _ -> fail "Unexpected media type"
  where filterByTitle = if Text.null title then id else filter (\l -> _linkTitle l == title)
        compareFeeds (Link _ _ (Just MediaTypeAtom{}) _) (Link _ _ (Just MediaTypeRSS{}) _)  = GT
        compareFeeds (Link _ _ (Just MediaTypeAtom{}) _) (Link _ _ (Just MediaTypeAtom{}) _) = EQ
        compareFeeds (Link _ _ (Just MediaTypeAtom{}) _) _                                   = GT
        compareFeeds (Link _ _ (Just MediaTypeRSS{}) _) (Link _ _ (Just MediaTypeAtom{}) _)  = LT
        compareFeeds (Link _ _ (Just MediaTypeRSS{}) _) (Link _ _ (Just MediaTypeRSS{}) _)   = EQ
        compareFeeds (Link _ _ (Just MediaTypeRSS{}) _) _                                    = GT
        compareFeeds _ _                                                                     = EQ


getHeader :: HeaderName -> Response a -> Maybe ByteString
getHeader name response = response & responseHeaders & filter (\h-> fst h == name) & map snd & listToMaybe
