{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
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
-- == Interpreter pattern
--
-- The behavior of this program can be customized through the interpreter pattern, implemented using free monads (for the DSL part) and cofree comonads (for the interpreter part).
--
-- The design is inspired from <http://dlaing.org/cofun/ Cofun with cofree monads>.
module Imm.Boot (imm) where

-- {{{ Imports
import qualified Imm.Core as Core
import Imm.Database.FeedTable as Database
import Imm.Database as Database
import Imm.Dyre as Dyre
import Imm.Feed
import Imm.HTTP as HTTP
import Imm.Hooks
import Imm.Logger as Logger
import Imm.Options as Options hiding(logLevel)
import Imm.Prelude
import Imm.Pretty
import Imm.XML

import Control.Comonad.Cofree
import Control.Monad.Trans.Free

import Data.Functor.Product
import Data.Functor.Sum

import System.IO (hFlush)
-- }}}

-- | Main function, meant to be used in your personal configuration file.
-- Each argument is an interpreter functor along with an initial state.
--
-- Here is an example:
--
-- > import           Imm.Boot
-- > import           Imm.Database.JsonFile
-- > import           Imm.Feed
-- > import           Imm.Hooks.SendMail
-- > import           Imm.HTTP.Simple
-- > import           Imm.Logger.Simple
-- > import           Imm.XML.Simple
-- >
-- > main :: IO ()
-- > main = do
-- >   logger   <- defaultLogger
-- >   manager  <- defaultManager
-- >   database <- defaultDatabase
-- >
-- >   imm (mkCoHttpClient, manager) (mkCoDatabase, database) (mkCoLogger, logger) (mkCoHooks, sendmail) (mkCoXmlParser, defaultPreProcess)
-- >
-- > sendmail :: SendMailSettings
-- > sendmail = SendMailSettings smtpServer formatMail
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
imm :: (a -> CoHttpClientF IO a, a)  -- ^ HTTP client interpreter (cf "Imm.HTTP")
    -> (b -> CoDatabaseF' IO b, b)   -- ^ Database interpreter (cf "Imm.Database")
    -> (c -> CoLoggerF IO c, c)      -- ^ Logger interpreter (cf "Imm.Logger")
    -> (d -> CoHooksF IO d, d)       -- ^ Hooks interpreter (cf "Imm.Hooks")
    -> (e -> CoXmlParserF IO e, e)   -- ^ XML parsing interpreter (cf "Imm.XML")
    -> IO ()
imm coHttpClient coDatabase coLogger coHooks coXmlParser = void $ do
  options <- parseOptions
  Dyre.wrap (optionDyreMode options) realMain (optionCommand options, optionLogLevel options, optionColorizeLogs options, coiter next start)
  where (next, start) = mkCoImm coHttpClient coDatabase coLogger coHooks coXmlParser

realMain :: (MonadIO m, PairingM (CoImmF m) ImmF m, MonadCatch m)
         => (Command, LogLevel, Bool, Cofree (CoImmF m) a) -> m ()
realMain (command, logLevel, colorizeLogs, interpreter) = void $ interpret (\_ b -> return b) interpreter $ do
  setColorizeLogs colorizeLogs
  setLogLevel logLevel
  logDebug . ("Dynamic reconfiguration settings:" <++>) . indent 2 =<< Dyre.describePaths
  logDebug $ "Executing: " <> pretty command
  logDebug . ("Using database:" <++>) . indent 2 =<< describeDatabase FeedTable

  handleAny (logError . textual . displayException) $ case command of
    Check t        -> Core.check            =<< resolveTarget ByPassConfirmation t
    Import         -> Core.importOPML
    Read t         -> mapM_ Database.markAsRead   =<< resolveTarget AskConfirmation t
    Run t          -> Core.run              =<< resolveTarget ByPassConfirmation t
    Show t         -> Core.showFeed         =<< resolveTarget ByPassConfirmation t
    ShowVersion    -> Core.printVersions
    Subscribe u c  -> Core.subscribe u c
    Unread t       -> mapM_ Database.markAsUnread =<< resolveTarget AskConfirmation t
    Unsubscribe t  -> Database.deleteList FeedTable =<< resolveTarget AskConfirmation t
    _              -> return ()

  Database.commit FeedTable
  flushLogs

-- * DSL/interpreter model

type CoImmF m = Product (CoHttpClientF m)
  (Product (CoDatabaseF' m)
   (Product (CoLoggerF m)
    (Product (CoHooksF m) (CoXmlParserF m)
    )))
type ImmF = Sum HttpClientF (Sum DatabaseF' (Sum LoggerF (Sum HooksF XmlParserF)))

mkCoImm :: (Functor m)
        => (a -> CoHttpClientF m a, a)
        -> (b -> CoDatabaseF' m b, b)
        -> (c -> CoLoggerF m c, c)
        -> (d -> CoHooksF m d, d)
        -> (e -> CoXmlParserF m e, e)
        -> ((a ::: b ::: c ::: d ::: e) -> CoImmF m (a ::: b ::: c ::: d ::: e), a ::: b ::: c ::: d ::: e)
mkCoImm (coHttpClient, a) (coDatabase, b) (coLogger, c) (coHooks, d) (coXmlParser, e) =
  (coHttpClient *: coDatabase *: coLogger *: coHooks *: coXmlParser, a +: b +: c +: d +: e)


-- * Util

data SafeGuard = AskConfirmation | ByPassConfirmation
  deriving(Eq, Read, Show)

data InterruptedException = InterruptedException deriving(Eq, Read, Show)
instance Exception InterruptedException where
  displayException _ = "Process interrupted"

promptConfirm :: (MonadIO m, MonadThrow m) => Text -> m ()
promptConfirm s = do
  hPut stdout $ s <> " Confirm [Y/n] "
  io $ hFlush stdout
  x <- getLine
  unless (null x || x == ("Y" :: Text)) $ throwM InterruptedException


resolveTarget :: (MonadIO m, MonadThrow m, MonadFree f m, DatabaseF' :<: f)
              => SafeGuard -> Maybe Core.FeedRef -> m [FeedID]
resolveTarget s Nothing = do
  result <- keys <$> Database.fetchAll FeedTable
  when (s == AskConfirmation) . promptConfirm $ "This will affect " <> show (length result) <> " feeds."
  return result
resolveTarget _ (Just (ByUID i)) = do
  result <- fst . (!! i) . mapToList <$> Database.fetchAll FeedTable
  -- logInfo $ "Target(s): " <> show (pretty result)
  return $ singleton result
resolveTarget _ (Just (ByURI uri)) = return [FeedID uri]
