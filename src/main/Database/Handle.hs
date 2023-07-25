{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module Database.Handle (
  -- * 1st-level primitives
  HandleF (..),
  Handle (..),
  interpret,
  DatabaseException (..),

  -- * 2nd-level primitives
  insertFeed,
  deleteFeed,
  insertItem,
  purge,
  commit,

  -- * 3rd-level primitives
  register,
  markItemAsProcessed,
  markFeedAsUnprocessed,
  markAllFeedsAsUnprocessed,

  -- * Re-exports
  module Database.Record,
) where

import Database.Record hiding (markFeedAsUnprocessed, markItemAsProcessed)
import qualified Database.Record as Record

import Control.Exception.Safe hiding (handle)
import Control.Monad.Time
import Imm.Feed
import Imm.Logger (LogLevel (..), log)
import qualified Imm.Logger as Logger
import Imm.Pretty
import URI.ByteString hiding (OtherError)

-- | 1st-level primitives
data Handle m = Handle
  { _describeDatabase ∷ m (Doc AnsiStyle)
  , _fetchAllFeeds ∷ m [FeedRecord Inserted]
  , _fetchFeed ∷ UID → m (FeedRecord Inserted)
  , _fetchAllItems ∷ m [FeedItemRecord Inserted]
  , _fetchItems ∷ UID → m [FeedItemRecord Inserted]
  , _fetchItem ∷ UID → m (FeedItemRecord Inserted)
  , _updateFeedDefinition ∷ FeedRecord Inserted → m ()
  , _updateFeedStatus ∷ FeedRecord Inserted → m ()
  , _updateItemStatus ∷ FeedItemRecord Inserted → m ()
  , _insertFeed ∷ FeedRecord NotInserted → m (FeedRecord Inserted)
  , _insertItem ∷ FeedItemRecord NotInserted → m (FeedItemRecord Inserted)
  , _deleteFeed ∷ UID → m ()
  , _purge ∷ m ()
  , _commit ∷ m ()
  }

-- | Database functor
data HandleF a
  = DescribeDatabase (Doc AnsiStyle → a)
  | FetchAllFeeds ([FeedRecord Inserted] → a)
  | FetchFeed UID (Either SomeException (FeedRecord Inserted) → a)
  | FetchAllItems ([FeedItemRecord Inserted] → a)
  | FetchItems UID ([FeedItemRecord Inserted] → a)
  | FetchItem UID (Either SomeException (FeedItemRecord Inserted) → a)
  | UpdateFeedDefinition (FeedRecord Inserted) a
  | UpdateFeedStatus (FeedRecord Inserted) a
  | UpdateItemStatus (FeedItemRecord Inserted) a
  | InsertFeed (FeedRecord NotInserted) (Either SomeException (FeedRecord Inserted) → a)
  | InsertItem (FeedItemRecord NotInserted) (FeedItemRecord Inserted → a)
  | DeleteFeed UID a
  | Purge a
  | Commit a
  deriving (Functor)

interpret ∷ MonadCatch m ⇒ Handle m → HandleF (m a) → m a
interpret database (DescribeDatabase f) = _describeDatabase database >>= f
interpret database (FetchAllFeeds f) = _fetchAllFeeds database >>= f
interpret database (FetchFeed uid f) = tryAny (_fetchFeed database uid) >>= f
interpret database (FetchAllItems f) = _fetchAllItems database >>= f
interpret database (FetchItems uid f) = _fetchItems database uid >>= f
interpret database (FetchItem uid f) = tryAny (_fetchItem database uid) >>= f
interpret database (UpdateFeedDefinition record f) = _updateFeedDefinition database record >> f
interpret database (UpdateFeedStatus record f) = _updateFeedStatus database record >> f
interpret database (UpdateItemStatus record f) = _updateItemStatus database record >> f
interpret database (InsertFeed record f) = tryAny (_insertFeed database record) >>= f
interpret database (InsertItem record f) = _insertItem database record >>= f
interpret database (DeleteFeed uid f) = _deleteFeed database uid >> f
interpret database (Purge f) = _purge database >> f
interpret database (Commit f) = _commit database >> f

data DatabaseException
  = NotCommitted
  | NotDeleted [FeedLocation]
  | FeedNotFound UID
  | FeedsNotInserted [FeedRecord NotInserted]
  | ItemsNotInserted [FeedItemRecord NotInserted]
  | NotPurged
  | NotUpdated FeedLocation
  | UnableFetchAll
  | InvalidURI URIParseError
  | OtherError Text
  deriving (Eq, Show)

instance Exception DatabaseException where
  displayException = show . pretty

instance Pretty DatabaseException where
  pretty NotCommitted = "Unable to commit database changes."
  pretty (NotDeleted x) = "Unable to delete the following entries in database:" <++> indent 2 (vsep $ map pretty x)
  pretty (FeedNotFound x) = "Unable to find feed in database using UID" <+> pretty x
  pretty (FeedsNotInserted x) = "Unable to insert the following feeds in database:" <++> indent 2 (vsep $ map (pretty . _feedLocation) x)
  pretty (ItemsNotInserted x) = "Unable to insert the following items in database:" <++> indent 2 (vsep $ map (pretty . _itemFeedKey) x)
  pretty NotPurged = "Unable to purge feed database"
  pretty (NotUpdated x) = "Unable to update the following entry in database:" <++> indent 2 (pretty x)
  pretty UnableFetchAll = "Unable to fetch all entries from database."
  pretty (InvalidURI e) = pretty $ show @Text e
  pretty (OtherError e) = "Other database error:" <+> pretty e

-- * 2nd-level primitives

insertFeed ∷ Monad m ⇒ Logger.Handle m → Handle m → FeedRecord NotInserted → m (FeedRecord Inserted)
insertFeed logger handle i = do
  log logger Info $ "Inserting feed" <+> magenta (prettyName $ _feedDefinition i)
  _insertFeed handle i

deleteFeed ∷ Monad m ⇒ Logger.Handle m → Handle m → UID → m ()
deleteFeed logger handle k = do
  log logger Info $ "Deleting feed" <+> magenta (pretty k) <+> "..."
  _deleteFeed handle k

insertItem ∷ Monad m ⇒ Logger.Handle m → Handle m → FeedItemRecord NotInserted → m (FeedItemRecord Inserted)
insertItem logger handle i = do
  log logger Info $ "Inserting item" <+> magenta (prettyName $ _itemDefinition i)
  _insertItem handle i

purge ∷ Monad m ⇒ Logger.Handle m → Handle m → m ()
purge logger handle = do
  log logger Info "Purging database..."
  _purge handle

commit ∷ Monad m ⇒ Logger.Handle m → Handle m → m ()
commit logger handle = do
  log logger Debug "Committing database transaction..."
  _commit handle
  log logger Debug "Database transaction committed"

-- * 3rd-level primitives

register ∷ MonadThrow m ⇒ Logger.Handle m → Handle m → FeedLocation → Set Text → m (FeedRecord Inserted)
register logger database feedLocation tags = do
  log logger Info $ "Registering feed" <+> magenta (pretty feedLocation) <> "..."
  insertFeed logger database $ mkFeedRecord feedLocation definition status
 where
  definition = FeedDefinition mempty
  status = FeedStatus tags mzero

markItemAsProcessed
  ∷ MonadThrow m
  ⇒ MonadTime m
  ⇒ Logger.Handle m
  → Handle m
  → FeedItemRecord Inserted
  → m ()
markItemAsProcessed logger database item = do
  log logger Debug $ "Marking item as processed:" <+> prettyName item <> "..."
  _updateItemStatus database $ item{_itemStatus = Record.markItemAsProcessed $ _itemStatus item}

markFeedAsUnprocessed ∷ MonadThrow m ⇒ Logger.Handle m → Handle m → UID → m ()
markFeedAsUnprocessed logger database key = do
  log logger Debug $ "Marking as unprocessed:" <+> pretty key <> "..."
  feed ← _fetchFeed database key
  items ← _fetchItems database key
  forM_ items $ \item → _updateItemStatus database $ item{_itemStatus = markItemAsUnprocessed $ _itemStatus item}
  _updateFeedStatus database $ feed{_feedStatus = Record.markFeedAsUnprocessed $ _feedStatus feed}

markAllFeedsAsUnprocessed ∷ MonadThrow m ⇒ Logger.Handle m → Handle m → m ()
markAllFeedsAsUnprocessed logger database = do
  log logger Debug "Marking all feeds as unprocessed..."
  feeds ← _fetchAllFeeds database
  items ← _fetchAllItems database
  forM_ items $ \item → _updateItemStatus database $ item{_itemStatus = markItemAsUnprocessed $ _itemStatus item}
  forM_ feeds $ \feed → _updateFeedStatus database $ feed{_feedStatus = Record.markFeedAsUnprocessed $ _feedStatus feed}
