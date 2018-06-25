{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | Database module abstracts over a key-value database that supports CRUD operations.
module Imm.Database where

-- {{{ Imports
import           Imm.Logger
import           Imm.Prelude
import           Imm.Pretty

import           Data.Map    (Map)
-- }}}

-- * Types

-- | Generic database table
class (Ord (Key t), Show (Key t), Show (Entry t), Typeable t, Show t, Pretty t, Pretty (Key t))
  => Table t where
  type Key t :: *
  type Entry t :: *

-- | Monad capable of interacting with a key-value store.
class MonadThrow m => MonadDatabase t m where
  _describeDatabase :: t -> m (Doc a)
  _fetchList :: t -> [Key t] -> m (Map (Key t) (Entry t))
  _fetchAll :: t -> m (Map (Key t) (Entry t))
  _update :: t -> Key t -> (Entry t -> Entry t) -> m ()
  _insertList :: t -> [(Key t, Entry t)] -> m ()
  _deleteList :: t -> [Key t] -> m ()
  _purge :: t -> m ()
  _commit :: t -> m ()


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

fetch :: (MonadDatabase t m, Table t, MonadThrow m) => t -> Key t -> m (Entry t)
fetch t k = do
  results <- _fetchList t [k]
  maybe (throwM $ NotFound t [k]) return $ lookup k results

fetchList :: (MonadDatabase t m, MonadThrow m) => t -> [Key t] -> m (Map (Key t) (Entry t))
fetchList = _fetchList

fetchAll :: (MonadThrow m, MonadDatabase t m) => t -> m (Map (Key t) (Entry t))
fetchAll = _fetchAll

update :: (MonadDatabase t m, MonadThrow m) => t -> Key t -> (Entry t -> Entry t) -> m ()
update  = _update

insert :: (MonadThrow m, MonadDatabase t m, MonadLog m) => t -> Key t -> Entry t -> m ()
insert t k v = insertList t [(k, v)]

insertList :: (MonadThrow m, MonadDatabase t m, MonadLog m) => t -> [(Key t, Entry t)] -> m ()
insertList t i = do
  logInfo $ "Inserting " <> yellow (pretty $ length i) <> " entries..."
  _insertList t i

delete :: (MonadThrow m, MonadDatabase t m, MonadLog m) => t -> Key t -> m ()
delete t k = deleteList t [k]

deleteList :: (MonadThrow m, MonadDatabase t m, MonadLog m) => t -> [Key t] -> m ()
deleteList t k = do
  logInfo $ "Deleting " <> yellow (pretty $ length k) <> " entries..."
  _deleteList t k

purge :: (MonadThrow m, MonadDatabase t m, MonadLog m) => t -> m ()
purge t = do
  logInfo "Purging database..."
  _purge t

commit :: (MonadThrow m, MonadDatabase t m, MonadLog m) => t -> m ()
commit t = do
  logDebug "Committing database transaction..."
  _commit t
  logDebug "Database transaction committed"
