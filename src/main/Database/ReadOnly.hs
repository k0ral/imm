{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Database.ReadOnly where

import Database.Handle
import Imm.Logger (LogLevel (..), log)
import qualified Imm.Logger as Logger
import Imm.Pretty

readOnly ∷ MonadFail m ⇒ Logger.Handle m → Handle m → Handle m
readOnly logger handle =
  handle
    { _describeDatabase = do
        output ← _describeDatabase handle
        return $ output <+> yellow (brackets "read only")
    , _updateFeedDefinition = \record → log logger Debug $ "Not updating feed" <+> prettyKey record
    , _updateFeedStatus = \record → log logger Debug $ "Not updating feed" <+> prettyKey record
    , _updateItemStatus = \record → log logger Debug $ "Not updating item" <+> prettyKey record
    , _insertFeed = \record → fail $ "Not inserting feed " <> show (prettyKey record)
    , _insertItem = \record → fail $ "Not inserting item " <> show (prettyItemRecord record)
    , _deleteFeed = \key → log logger Debug $ "Not deleting feed" <+> pretty key
    , _purge = log logger Debug "Not purging database"
    , _commit = log logger Debug "Not committing database"
    }
