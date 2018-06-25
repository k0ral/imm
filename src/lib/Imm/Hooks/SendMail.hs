{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
-- | Implementation of "Imm.Hooks" that sends a mail via a SMTP server for each new RSS/Atom element.
-- You may want to check out "Network.HaskellNet.SMTP", "Network.HaskellNet.SMTP.SSL" and "Network.Mail.Mime" modules for additional information.
--
-- Here is an example configuration:
--
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
-- >   (StartTls "smtp.server" defaultSettingsSMTPSTARTTLS)
--
module Imm.Hooks.SendMail (module Imm.Hooks.SendMail, module Reexport) where

-- {{{ Imports
import           Imm.Feed
import           Imm.Hooks
import           Imm.Prelude
import           Imm.Pretty

import           Control.Monad.Trans.Reader
import           Data.NonNull
import           Data.Time
import           Network.HaskellNet.SMTP     as Reexport
import           Network.HaskellNet.SMTP.SSL as Reexport
import           Network.Mail.Mime           as Reexport hiding (sendmail)
import           Network.Socket
import           Text.Atom.Types
import           Text.RSS.Types
-- }}}

-- * Types

type Username = String
type Password = String
type ServerName = String

-- | How to connect to the SMTP server
data ConnectionSettings = Plain ServerName PortNumber | Ssl ServerName Settings | StartTls ServerName Settings
  deriving(Eq, Show)

-- | How to authenticate to the SMTP server
data Authentication = Authentication AuthType Username Password
  deriving(Eq, Show)

data SMTPServer = SMTPServer (Maybe Authentication) ConnectionSettings
  deriving (Eq, Show)

-- | How to format outgoing mails from feed elements
data FormatMail = FormatMail
  { formatFrom    :: Feed -> FeedElement -> Address    -- ^ How to write the From: header of feed mails
  , formatSubject :: Feed -> FeedElement -> Text       -- ^ How to write the Subject: header of feed mails
  , formatBody    :: Feed -> FeedElement -> Text       -- ^ How to write the body of feed mails (sic!)
  , formatTo      :: Feed -> FeedElement -> [Address]  -- ^ How to write the To: header of feed mails
  }

data SendMailSettings = SendMailSettings (Feed -> FeedElement -> SMTPServer) FormatMail

instance MonadImm (ReaderT SendMailSettings IO) where
  processNewElement feed element = do
    SendMailSettings connectionSettings formatMail <- ask
    timezone <- lift getCurrentTimeZone
    currentTime <- lift getCurrentTime
    let mail = buildMail formatMail currentTime timezone feed element
    lift $ withSMTPConnection (connectionSettings feed element) $ sendMimeMail2 mail


-- * Default behavior

-- | Fill 'addressName' with the feed title and, if available, the authors' names.
--
-- This function leaves 'addressEmail' empty. You are expected to fill it adequately, because many SMTP servers enforce constraints on the From: email.
defaultFormatFrom :: Feed -> FeedElement -> Address
defaultFormatFrom (Rss doc) (RssElement item) = Address (Just $ channelTitle doc <> " (" <> itemAuthor item <> ")") ""
defaultFormatFrom (Atom feed) (AtomElement entry) = Address (Just $ title <> " (" <> authors <> ")") ""
  where title = show . prettyAtomText $ feedTitle feed
        authors = intercalate ", " $ map (toNullable . personName) $ entryAuthors entry <> feedAuthors feed
defaultFormatFrom _ _ = Address (Just "Unknown") ""

-- | Fill mail subject with the element title
defaultFormatSubject :: Feed -> FeedElement -> Text
defaultFormatSubject _ = getTitle

-- | Fill mail body with:
--
-- - a list of links associated to the element
-- - the element's content or description/summary
defaultFormatBody :: Feed -> FeedElement -> Text
defaultFormatBody _ (RssElement item) = "<p>" <> maybe "<no link>" (withRssURI (show . prettyURI)) (itemLink item) <> "</p><p>" <> itemDescription item <> "</p>"
defaultFormatBody _ (AtomElement entry) = "<p>" <> intercalate "<br/>" links <> "</p><p>" <> fromMaybe "<empty>" (content <|> summary) <> "</p>"
  where links   = map (withAtomURI (show . prettyURI) . linkHref) $ entryLinks entry
        content = show . prettyAtomContent <$> entryContent entry
        summary = show . prettyAtomText <$> entrySummary entry


-- * Low-level helpers

authenticate_ :: SMTPConnection -> Authentication -> IO Bool
authenticate_ connection (Authentication t u p) = do
  result <- authenticate t u p connection
  unless result $ putStrLn "Authentication failed"
  return result

withSMTPConnection :: SMTPServer -> (SMTPConnection -> IO a) -> IO a
withSMTPConnection (SMTPServer authentication (Plain server port)) f =
  doSMTPPort server port $ \connection -> do
    forM_ authentication (authenticate_ connection)
    f connection
withSMTPConnection (SMTPServer authentication (Ssl server settings)) f =
  doSMTPSSLWithSettings server settings $ \connection -> do
    forM_ authentication (authenticate_ connection)
    f connection
withSMTPConnection (SMTPServer authentication (StartTls server settings)) f =
  doSMTPSTARTTLSWithSettings server settings $ \connection -> do
    forM_ authentication (authenticate_ connection)
    f connection

-- | Build mail from a given feed
buildMail :: FormatMail -> UTCTime -> TimeZone -> Feed -> FeedElement -> Mail
buildMail format currentTime timeZone feed element =
  let date = formatTime defaultTimeLocale "%a, %e %b %Y %T %z" $ utcToZonedTime timeZone $ fromMaybe currentTime $ getDate element
  in Mail
    { mailFrom = formatFrom format feed element
    , mailTo   = formatTo format feed element
    , mailCc   = []
    , mailBcc  = []
    , mailHeaders =
        [ ("Return-Path", "<imm@noreply>")
        , ("Date", fromString date)
        , ("Subject", formatSubject format feed element)
        , ("Content-disposition", "inline")
        ]
    , mailParts = [[htmlPart $ fromStrict $ formatBody format feed element]]
    }
