{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | Database module abstracts over a key-value database that supports CRUD operations.
--
-- This module follows the [Handle pattern](https://jaspervdj.be/posts/2018-03-08-handle-pattern.html).
--
-- > import qualified Imm.Database as Database
module Imm.Database where

-- {{{ Imports
import           Imm.Logger             hiding (Handle)
import qualified Imm.Logger             as Logger
import           Imm.Pretty

import           Control.Exception.Safe hiding (handle)
import           Data.Map               (Map)
-- }}}

-- * Types

-- | Generic database table
class (Ord (Key t), Show (Key t), Show (Entry t), Typeable t, Show t, Pretty t, Pretty (Key t))
  => Table t where
  type Key t :: *
  type Entry t :: *
  rep :: t

data Handle m t = Handle
  { _describeDatabase :: m (Doc AnsiStyle)
  , _fetchList        :: [Key t] -> m (Map (Key t) (Entry t))
  , _fetchAll         :: m (Map (Key t) (Entry t))
  , _update           :: Key t -> (Entry t -> Entry t) -> m ()
  , _insertList       :: [(Key t, Entry t)] -> m ()
  , _deleteList       :: [Key t] -> m ()
  , _purge            :: m ()
  , _commit           :: m ()
  }

readOnly :: Monad m => Pretty (Key t) => Logger.Handle m -> Handle m t -> Handle m t
readOnly logger handle = handle
  { _describeDatabase = do
      output <- _describeDatabase handle
      return $ output <+> yellow (brackets "read only")
  , _update = \key _ -> log logger Debug $ "Not updating database for key " <> pretty key
  , _insertList = \list -> log logger Debug $ "Not inserting " <> yellow (pretty $ length list) <> " entries"
  , _deleteList = \list -> log logger Debug $ "Not deleting " <> yellow (pretty $ length list) <> " entries"
  , _purge = log logger Debug "Not purging database"
  , _commit = log logger Debug "Not committing database"
  }



data DatabaseException t
  = NotCommitted t
  | NotDeleted t [Key t]
  | NotFound t [Key t]
  | NotInserted t [(Key t, Entry t)]
  | NotPurged t
  | NotUpdated t (Key t)
  | UnableFetchAll t

deriving instance (Eq t, Eq (Key t), Eq (Entry t)) => Eq (DatabaseException t)
deriving instance (Show t, Show (Key t), Show (Entry t)) => Show (DatabaseException t)

instance (Table t, Show (Key t), Show (Entry t), Pretty (Key t), Typeable t) => Exception (DatabaseException t) where
  displayException = show . pretty

instance (Pretty t, Pretty (Key t)) => Pretty (DatabaseException t) where
  pretty (NotCommitted _) = "Unable to commit database changes."
  pretty (NotDeleted _ x) = "Unable to delete the following entries in database:" <++> indent 2 (vsep $ map pretty x)
  pretty (NotFound _ x) = "Unable to find the following entries in database:" <++> indent 2 (vsep $ map pretty x)
  pretty (NotInserted _ x) = "Unable to insert the following entries in database:" <++> indent 2 (vsep $ map (pretty . fst) x)
  pretty (NotPurged t) = "Unable to purge database" <+> pretty t
  pretty (NotUpdated _ x) = "Unable to update the following entry in database:" <++> indent 2 (pretty x)
  pretty (UnableFetchAll _) = "Unable to fetch all entries from database."


-- * Primitives

fetch :: Monad m => Table t => MonadThrow m => Handle m t -> Key t -> m (Entry t)
fetch handle k = do
  results <- _fetchList handle [k]
  maybe (throwM $ NotFound (table handle) [k]) return $ lookup k results

fetchList :: Monad m => Handle m t -> [Key t] -> m (Map (Key t) (Entry t))
fetchList = _fetchList

fetchAll :: Monad m => Handle m t -> m (Map (Key t) (Entry t))
fetchAll = _fetchAll

update :: Monad m => Handle m t -> Key t -> (Entry t -> Entry t) -> m ()
update  = _update

insert :: Monad m => Logger.Handle m -> Handle m t -> Key t -> Entry t -> m ()
insert logger handle k v = insertList logger handle [(k, v)]

insertList :: Monad m => Logger.Handle m -> Handle m t -> [(Key t, Entry t)] -> m ()
insertList logger handle i = do
  log logger Info $ "Inserting " <> yellow (pretty $ length i) <> " entries..."
  _insertList handle i

delete :: Monad m => Logger.Handle m -> Handle m t -> Key t -> m ()
delete logger handle k = deleteList logger handle [k]

deleteList :: Monad m => Logger.Handle m -> Handle m t -> [Key t] -> m ()
deleteList logger handle k = do
  log logger Info $ "Deleting " <> yellow (pretty $ length k) <> " entries..."
  _deleteList handle k

purge :: Monad m => Logger.Handle m -> Handle m t -> m ()
purge logger handle = do
  log logger Info "Purging database..."
  _purge handle

commit :: Monad m => Logger.Handle m -> Handle m t -> m ()
commit logger handle = do
  log logger Debug "Committing database transaction..."
  _commit handle
  log logger Debug "Database transaction committed"

table :: Table t => Handle m t -> t
table _ = rep
