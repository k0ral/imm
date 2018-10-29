{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
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
-- == @ReaderT@ pattern
--
-- The behavior of this program can be customized through the @ReaderT@ pattern.
module Imm.Boot (imm, Modules(..), ModulesM, mkModulesM) where

-- {{{ Imports
import qualified Imm.Core                   as Core
import           Imm.Database               as Database
import           Imm.Database.FeedTable     as Database
import           Imm.Dyre                   as Dyre
import           Imm.Feed
import           Imm.Hooks
import           Imm.HTTP                   as HTTP
import           Imm.Logger                 as Logger
import           Imm.Options                as Options hiding (logLevel)
import           Imm.Prelude
import           Imm.Pretty
import           Imm.XML

import           Control.Monad.Time
import           Control.Monad.Trans.Reader
import           Data.Conduit.Combinators   (stdin)
import           Streamly                   (MonadAsync)
import           System.IO                  (hFlush)
-- }}}

-- | Modules are independent features of the program which behavior can be controlled by the user.
data Modules httpClient databaseClient logger hooks xmlParser = Modules
  { _httpClient     :: httpClient      -- ^ HTTP client interpreter (cf "Imm.HTTP")
  , _databaseClient :: databaseClient  -- ^ Database interpreter (cf "Imm.Database")
  , _logger         :: logger          -- ^ Logging interpreter (cf "Imm.Logger")
  , _hooks          :: hooks           -- ^ Hooks interpreter (cf "Imm.Hooks")
  , _xmlParser      :: xmlParser       -- ^ XML parsing interpreter (cf "Imm.XML")
  }

-- | Type-erased version of 'Modules', using existential quantification.
data ModulesM m = forall a b c d e .
  ( MonadHttpClient (ReaderT a m)
  , MonadDatabase FeedTable (ReaderT b m)
  , MonadLog (ReaderT c m)
  , MonadImm (ReaderT d m)
  , MonadXmlParser (ReaderT e m)
  ) => ModulesM (Modules a b c d e)

-- | Constructor for 'ModulesM'.
mkModulesM :: (MonadXmlParser (ReaderT e m), MonadImm (ReaderT d m), MonadLog (ReaderT c m), MonadDatabase FeedTable (ReaderT b m), MonadHttpClient (ReaderT a m))
           => a -> b -> c -> d -> e -> ModulesM m
mkModulesM a b c d e = ModulesM $ Modules a b c d e


instance (MonadIO m, MonadLog (ReaderT c m)) => MonadLog (ReaderT (Modules a b c d e) m) where
  log l t = withReaderT _logger $ log l t
  getLogLevel = withReaderT _logger getLogLevel
  setLogLevel l = withReaderT _logger $ setLogLevel l
  setColorizeLogs c = withReaderT _logger $ setColorizeLogs c
  flushLogs = withReaderT _logger flushLogs

instance (Monad m, MonadImm (ReaderT d m)) => MonadImm (ReaderT (Modules a b c d e) m) where
  processNewElement feed element = withReaderT _hooks $ processNewElement feed element

instance (MonadThrow m, MonadHttpClient (ReaderT a m)) => MonadHttpClient (ReaderT (Modules a b c d e) m) where
  httpGet uri = withReaderT _httpClient $ httpGet uri

instance (MonadThrow m, MonadXmlParser (ReaderT e m))
  => MonadXmlParser (ReaderT (Modules a b c d e) m) where
  parseXml uri bytes = withReaderT _xmlParser $ parseXml uri bytes

instance (MonadThrow m, MonadDatabase FeedTable (ReaderT b m))
  => MonadDatabase FeedTable (ReaderT (Modules a b c d e) m) where
  _describeDatabase t = withReaderT _databaseClient $ _describeDatabase t
  _fetchList t k = withReaderT _databaseClient $ _fetchList t k
  _fetchAll t = withReaderT _databaseClient $ _fetchAll t
  _update t key f = withReaderT _databaseClient $ _update t key f
  _insertList t list = withReaderT _databaseClient $ _insertList t list
  _deleteList t k = withReaderT _databaseClient $ _deleteList t k
  _purge t = withReaderT _databaseClient $ _purge t
  _commit t = withReaderT _databaseClient $ _commit t


-- | Main function, meant to be used in your personal configuration file.
--
-- Here is an example:
--
-- > import           Imm.Boot
-- > import           Imm.Database.JsonFile
-- > import           Imm.Feed
-- > import           Imm.Hooks.SendMail
-- > import           Imm.HTTP.Conduit
-- > import           Imm.Logger.Simple
-- > import           Imm.XML.Simple
-- >
-- > main :: IO ()
-- > main = do
-- >   logger   <- defaultLogger
-- >   manager  <- defaultManager
-- >   database <- defaultDatabase
-- >
-- >   imm $ mkModulesM manager database logger sendmail defaultXmlParser
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
imm :: ModulesM IO -> IO ()
imm modules = void $ do
  options <- parseOptions
  Dyre.wrap (optionDyreMode options) realMain (optionCommand options, optionLogLevel options, optionColorizeLogs options, modules)

realMain :: (MonadAsync m, MonadTime m, MonadCatch m)
         => (Command, LogLevel, Bool, ModulesM m) -> m ()
realMain (command, logLevel, enableColors, ModulesM modules) = void $ flip runReaderT modules $ do
  setColorizeLogs enableColors
  setLogLevel logLevel
  logDebug . ("Dynamic reconfiguration settings:" <++>) . indent 2 =<< Dyre.describePaths
  logDebug $ "Executing: " <> pretty command
  logDebug . ("Using database:" <++>) . indent 2 =<< _describeDatabase FeedTable

  handleAny (logError . pretty . displayException) $ case command of
    Check t        -> Core.check =<< resolveTarget ByPassConfirmation t
    Help           -> liftBase $ putStrLn helpString
    Import         -> Core.importOPML stdin
    Read t         -> mapM_ Database.markAsRead =<< resolveTarget AskConfirmation t
    Run t          -> Core.run =<< resolveTarget ByPassConfirmation t
    Show t         -> Core.showFeed =<< resolveTarget ByPassConfirmation t
    ShowVersion    -> Core.printVersions
    Subscribe u c  -> Core.subscribe u c
    Unread t       -> mapM_ Database.markAsUnread =<< resolveTarget AskConfirmation t
    Unsubscribe t  -> Database.deleteList FeedTable =<< resolveTarget AskConfirmation t
    _              -> return ()

  Database.commit FeedTable
  flushLogs


-- * Util

data SafeGuard = AskConfirmation | ByPassConfirmation
  deriving(Eq, Read, Show)

data InterruptedException = InterruptedException deriving(Eq, Read, Show)
instance Exception InterruptedException where
  displayException _ = "Process interrupted"

promptConfirm :: Text -> IO ()
promptConfirm s = do
  putStr $ s <> " Confirm [Y/n] "
  hFlush stdout
  x <- getLine
  unless (null x || x == ("Y" :: Text)) $ throwM InterruptedException


resolveTarget :: (MonadBase IO m, MonadThrow m, MonadDatabase FeedTable m)
              => SafeGuard -> Maybe Core.FeedRef -> m [FeedID]
resolveTarget s Nothing = do
  result <- keys <$> Database.fetchAll FeedTable
  when (s == AskConfirmation) $ liftBase $ promptConfirm $ "This will affect " <> show (length result) <> " feeds."
  return result
resolveTarget _ (Just (ByUID i)) = do
  result <- fst . (!! (i-1)) . mapToList <$> Database.fetchAll FeedTable
  -- logInfo $ "Target(s): " <> show (pretty result)
  return $ singleton result
resolveTarget _ (Just (ByURI uri)) = return [FeedID uri]
