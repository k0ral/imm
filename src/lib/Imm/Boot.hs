{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
-- |
-- = Getting started
--
-- == Dynamic reconfiguration
--
-- This program is dynamically configured using the <https://hackage.haskell.org/package/dyre dyre> library.
--
-- You may want to check out <https://hackage.haskell.org/package/dyre/docs/Config-Dyre.html this documentation> to know how to get started.
--
-- Your personal configuration is located at @$XDG_CONFIG_HOME\/imm\/imm.hs@.
--
-- == Handle pattern
--
-- The behavior of this program can be customized through the [Handle pattern](https://jaspervdj.be/posts/2018-03-08-handle-pattern.html).
module Imm.Boot (imm) where

-- {{{ Imports
import qualified Imm.Core                 as Core
import           Imm.Database             as Database
import           Imm.Database.FeedTable   as Database
import           Imm.Dyre                 as Dyre
import           Imm.Feed
import           Imm.Hooks                as Hooks
import           Imm.HTTP                 as HTTP
import           Imm.Logger               as Logger
import           Imm.Options              as Options hiding (logLevel)
import           Imm.Pretty
import           Imm.XML                  as XML

import           Control.Exception.Safe
import           Data.Conduit.Combinators as Conduit (stdin)
import qualified Data.Map                 as Map
import           Data.Text                as Text hiding (length)
import           Data.Text.IO             as Text
import           Relude.Unsafe            (at)
import           System.IO                (hFlush)
-- }}}

-- | Main function, meant to be used in your personal configuration file.
--
-- Here is an example:
--
-- > import           Imm.Boot
-- > import           Imm.Database.JsonFile as Database
-- > import           Imm.Feed
-- > import           Imm.Hooks.SendMail as Hooks
-- > import           Imm.HTTP.Simple as HTTP
-- > import           Imm.Logger.Simple as Logger
-- > import           Imm.XML.Conduit as XML
-- >
-- > main :: IO ()
-- > main = do
-- >   logger     <- Logger.mkHandle <$> defaultLogger
-- >   database   <- Database.mkHandle <$> defaultDatabase
-- >   httpClient <- HTTP.mkHandle <$> defaultManager
-- >
-- >   imm logger database httpClient hooks xmlParser
-- >
-- > xmlParser :: XML.Handle IO
-- > xmlParser = XML.mkHandle defaultXmlParser
-- >
-- > hooks :: Hooks.Handle IO
-- > hooks = Hooks.mkHandle $ SendMailSettings smtpServer formatMail
-- >
-- > formatMail :: FormatMail
-- > formatMail = FormatMail
-- >   (\a b -> (defaultFormatFrom a b) { addressEmail = "user@host" } )
-- >   defaultFormatSubject
-- >   defaultFormatBody
-- >   (\_ _ -> [Address Nothing "user@host"])
-- >
-- > smtpServer :: Feed -> FeedElement -> SMTPServer
-- > smtpServer _ _ = SMTPServer
-- >   (Just $ Authentication PLAIN "user" "password")
-- >   (StartTls "smtp.host" defaultSettingsSMTPSTARTTLS)
imm :: Logger.Handle IO -> Database.Handle IO FeedTable -> HTTP.Handle IO -> Hooks.Handle IO -> XML.Handle IO -> IO ()
imm logger database httpClient hooks xmlParser = void $ do
  options <- parseOptions
  Dyre.wrap (optionDyreMode options) realMain (optionCommand options, optionLogLevel options, optionColorizeLogs options, logger, database, httpClient, hooks, xmlParser)

realMain :: (Command, LogLevel, Bool, Logger.Handle IO, Database.Handle IO FeedTable, HTTP.Handle IO, Hooks.Handle IO, XML.Handle IO) -> IO ()
realMain (command, logLevel, enableColors, logger, database, httpClient, hooks, xmlParser) = void $ do
  setColorizeLogs logger enableColors
  setLogLevel logger logLevel
  log logger Debug . ("Dynamic reconfiguration settings:" <++>) . indent 2 =<< Dyre.describePaths
  log logger Debug $ "Executing: " <> pretty command
  log logger Debug . ("Using database:" <++>) . indent 2 =<< _describeDatabase database

  handleAny (log logger Error . pretty . displayException) $ case command of
    Check t        -> Core.check logger database httpClient xmlParser =<< resolveTarget database ByPassConfirmation t
    Help           -> Text.putStrLn helpString
    Import         -> Core.importOPML logger database Conduit.stdin
    Read t         -> mapM_ (Database.markAsRead logger database) =<< resolveTarget database AskConfirmation t
    Run t          -> Core.run logger database httpClient hooks xmlParser =<< resolveTarget database ByPassConfirmation t
    Show t         -> Core.showFeed logger database =<< resolveTarget database ByPassConfirmation t
    ShowVersion    -> Core.printVersions
    Subscribe u c  -> Core.subscribe logger database u c
    Unread t       -> mapM_ (Database.markAsUnread logger database) =<< resolveTarget database AskConfirmation t
    Unsubscribe t  -> Database.deleteList logger database =<< resolveTarget database AskConfirmation t
    _              -> return ()

  Database.commit logger database
  flushLogs logger


-- * Util

data SafeGuard = AskConfirmation | ByPassConfirmation
  deriving(Eq, Read, Show)

data InterruptedException = InterruptedException deriving(Eq, Read, Show)
instance Exception InterruptedException where
  displayException _ = "Process interrupted"

promptConfirm :: Text -> IO ()
promptConfirm s = do
  Text.putStr $ s <> " Confirm [Y/n] "
  hFlush stdout
  x <- Text.getLine
  unless (Text.null x || x == "Y") $ throwM InterruptedException


resolveTarget :: MonadIO m => MonadThrow m => Database.Handle m FeedTable -> SafeGuard -> Maybe Core.FeedRef -> m [FeedID]
resolveTarget database s Nothing = do
  result <- Map.keys <$> Database.fetchAll database
  when (s == AskConfirmation) $ liftIO $ promptConfirm $ "This will affect " <> show (length result) <> " feeds."
  return result
resolveTarget database _ (Just (ByUID i)) = do
  result <- fst . at (i-1) . Map.toList <$> Database.fetchAll database
  -- log logger Info $ "Target(s): " <> show (pretty result)
  return [result]
resolveTarget _ _ (Just (ByURI uri)) = return [FeedID uri]
