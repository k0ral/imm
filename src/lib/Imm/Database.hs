{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | DSL/interpreter model for a generic key-value database
module Imm.Database where

-- {{{ Imports
import           Imm.Error
import           Imm.Logger
import           Imm.Prelude

import           Control.Monad.Trans.Free

import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))
-- }}}

-- * DSL/interpreter

-- | Generic database table
class (Ord (Key t), Show (Key t), Show (Entry t), Typeable t, Show t, Pretty t, Pretty (Key t), Pretty (Entry t))
  => Table t where
  type Key t :: *
  type Entry t :: *

-- | Database DSL
data DatabaseF t next
  = Describe t (Doc -> next)
  | FetchList t [Key t] (Either SomeException (Map (Key t) (Entry t)) -> next)
  | FetchAll t (Either SomeException (Map (Key t) (Entry t)) -> next)
  | Update t (Key t) (Entry t -> Entry t) (Either SomeException () -> next)
  | InsertList t [(Key t, Entry t)] (Either SomeException () -> next)
  | DeleteList t [Key t] (Either SomeException () -> next)
  | Purge t (Either SomeException () -> next)
  | Commit t (Either SomeException () -> next)
  deriving(Functor)

-- | Database interpreter
data CoDatabaseF t m a = CoDatabaseF
  { describeH   :: m (Doc, a)
  , fetchListH  :: [Key t] -> m (Either SomeException (Map (Key t) (Entry t)), a)
  , fetchAllH   :: m (Either SomeException (Map (Key t) (Entry t)), a)
  , updateH     :: Key t -> (Entry t -> Entry t) -> m (Either SomeException (), a)
  , insertListH :: [(Key t, Entry t)] -> m (Either SomeException (), a)
  , deleteListH :: [Key t] -> m (Either SomeException (), a)
  , purgeH      :: m (Either SomeException (), a)
  , commitH     :: m (Either SomeException (), a)
  } deriving(Functor)

instance Monad m => PairingM (CoDatabaseF t m) (DatabaseF t) m where
  -- pairM :: (a -> b -> m r) -> f a -> g b -> m r
  pairM p CoDatabaseF{describeH} (Describe _ next) = do
    (result, a) <- describeH
    p a $ next result
  pairM p CoDatabaseF{fetchListH} (FetchList _ key next) = do
    (result, a) <- fetchListH key
    p a $ next result
  pairM p CoDatabaseF{fetchAllH} (FetchAll _ next) = do
    (result, a) <- fetchAllH
    p a $ next result
  pairM p CoDatabaseF{updateH} (Update _ key f next) = do
    (result, a) <- updateH key f
    p a $ next result
  pairM p CoDatabaseF{insertListH} (InsertList _ rows next) = do
    (result, a) <- insertListH rows
    p a $ next result
  pairM p CoDatabaseF{deleteListH} (DeleteList _ k next) = do
    (result, a) <- deleteListH k
    p a $ next result
  pairM p CoDatabaseF{purgeH} (Purge _ next) = do
    (result, a) <- purgeH
    p a $ next result
  pairM p CoDatabaseF{commitH} (Commit _ next) = do
    (result, a) <- commitH
    p a $ next result


-- * Exception

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
  pretty (NotCommitted _) = text "Unable to commit database changes."
  pretty (NotDeleted _ x) = text "Unable to delete the following entries in database:" <++> indent 2 (vsep $ map pretty x)
  pretty (NotFound _ x) = text "Unable to find the following entries in database:" <++> indent 2 (vsep $ map pretty x)
  pretty (NotInserted _ x) = text "Unable to insert the following entries in database:" <++> indent 2 (vsep $ map (pretty . fst) x)
  pretty (NotPurged t) = text "Unable to purge database" <+> pretty t
  pretty (NotUpdated _ x) = text "Unable to update the following entry in database:" <++> indent 2 (pretty x)
  pretty (UnableFetchAll _) = text "Unable to fetch all entries from database."


-- * Primitives

describeDatabase :: (MonadFree f m, DatabaseF t :<: f)
                 => t -> m Doc
describeDatabase t = liftF . inj $ Describe t id

fetch :: (MonadFree f m, DatabaseF t :<: f, Table t, MonadThrow m)
      => t -> Key t -> m (Entry t)
fetch t k = do
  results <- liftF . inj $ FetchList t [k] id
  result <- lookup k <$> liftE results
  maybe (throwM $ NotFound t [k]) return result

fetchList :: (MonadFree f m, DatabaseF t :<: f, MonadThrow m)
          => t -> [Key t] -> m (Map (Key t) (Entry t))
fetchList t k = do
  result <- liftF . inj $ FetchList t k id
  liftE result

fetchAll :: (MonadThrow m, MonadFree f m, DatabaseF t :<: f) => t -> m (Map (Key t) (Entry t))
fetchAll t = do
  result <- liftF . inj $ FetchAll t id
  liftE result

update :: (MonadFree f m, DatabaseF t :<: f, MonadThrow m)
       => t -> Key t -> (Entry t -> Entry t) -> m ()
update t k f = do
  result <- liftF . inj $ Update t k f id
  liftE result

insert :: (MonadThrow m, MonadFree f m, LoggerF :<: f, DatabaseF t :<: f)
       => t -> Key t -> Entry t -> m ()
insert t k v = insertList t [(k, v)]

insertList :: (MonadThrow m, MonadFree f m, LoggerF :<: f, DatabaseF t :<: f)
           => t -> [(Key t, Entry t)] -> m ()
insertList t i = do
  logInfo $ "Inserting " <> yellow (pretty $ length i) <> " entries..."
  result <- liftF . inj $ InsertList t i id
  liftE result

delete :: (MonadThrow m, MonadFree f m, LoggerF :<: f, DatabaseF t :<: f) => t -> Key t -> m ()
delete t k = deleteList t [k]

deleteList :: (MonadThrow m, MonadFree f m, LoggerF :<: f, DatabaseF t :<: f)
           => t -> [Key t] -> m ()
deleteList t k = do
  logInfo $ "Deleting " <> yellow (pretty $ length k) <> " entries..."
  result <- liftF . inj $ DeleteList t k id
  liftE result

purge :: (MonadThrow m, MonadFree f m, DatabaseF t :<: f, LoggerF :<: f) => t -> m ()
purge t = do
  logInfo "Purging database..."
  result <- liftF . inj $ Purge t id
  liftE result

commit :: (MonadThrow m, MonadFree f m, DatabaseF t :<: f, LoggerF :<: f) => t -> m ()
commit t = do
  logDebug "Committing database transaction..."
  result <- liftF . inj $ Commit t id
  liftE result
  logDebug "Database transaction committed"
