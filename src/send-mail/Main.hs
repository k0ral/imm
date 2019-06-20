{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- | 'Callback' for @imm@ that sends a mail via a SMTP server the input RSS/Atom item.
-- {{{ Imports
import           Imm.Callback
import           Imm.Feed
import           Imm.Pretty

import           Data.ByteString             (getContents)
import qualified Data.MessagePack            as MsgPack
import           Data.Text                   as Text (intercalate)
import           Data.Time
import           Network.HaskellNet.SMTP
import           Network.HaskellNet.SMTP.SSL
import           Network.Mail.Mime           hiding (sendmail)
import           Network.Socket
import           Options.Applicative
import           Refined
import           Text.Atom.Types
import           Text.RSS.Types
-- }}}

-- * Types

type Username = String
type Password = String
type ServerName = String

-- | How to connect to the SMTP server
data ConnectionSettings = Plain ServerName PortNumber | Encrypted ServerName Settings Bool
  deriving(Eq, Generic, Ord, Show)

-- | How to authenticate to the SMTP server
data Authentication = Authentication AuthType Username Password
  deriving(Eq, Generic, Show)

data SMTPServer = SMTPServer (Maybe Authentication) ConnectionSettings
  deriving (Eq, Generic, Show)

-- | How to format outgoing mails from feed elements
data FormatMail = FormatMail
  { formatFrom    :: Feed -> FeedElement -> Address    -- ^ How to write the From: header of feed mails
  , formatSubject :: Feed -> FeedElement -> Text       -- ^ How to write the Subject: header of feed mails
  , formatBody    :: Feed -> FeedElement -> Text       -- ^ How to write the body of feed mails (sic!)
  , formatTo      :: Feed -> FeedElement -> [Address]  -- ^ How to write the To: header of feed mails
  }

data CliOptions = CliOptions
  { _smtpServer :: SMTPServer
  , _recipients :: [Address]
  , _dryRun     :: Bool
  } deriving (Eq, Generic, Show)


parseOptions :: MonadIO m => m CliOptions
parseOptions = io $ customExecParser (prefs $ showHelpOnError <> showHelpOnEmpty) (info (cliOptions <**> helper) $ progDesc "Send a mail for each new RSS/Atom item.")

cliOptions :: Parser CliOptions
cliOptions = CliOptions
  <$> smtpServerParser
  <*> many recipientParser
  <*> switch (long "dry-run" <> help "Disable all I/Os, except for logs.")

smtpServerParser :: Parser SMTPServer
smtpServerParser = SMTPServer
  <$> ((Just <$> authenticationParser) <|> pure Nothing)
  <*> (plainConnection <|> encryptedConnection)

authenticationParser :: Parser Authentication
authenticationParser = Authentication
  <$> authenticationType
  <*> strOption (long "user" <> short 'u' <> help "SMTP username")
  <*> strOption (long "password" <> short 'P' <> help "SMTP password")

authenticationType :: Parser AuthType
authenticationType = flag' PLAIN (long "plain" <> help "Use plain authentication.")
  <|> flag' LOGIN (long "login" <> help "Use login authentication")
  <|> flag' CRAM_MD5 (long "cram-md5" <> help "Use CRAM MD5 authentication")

plainConnection :: Parser ConnectionSettings
plainConnection = Plain
  <$> strOption (long "server" <> short 's' <> help "SMTP server address.")
  <*> option auto (long "port" <> short 'p' <> help "SMTP server port.")

encryptedConnection :: Parser ConnectionSettings
encryptedConnection = Encrypted
  <$> strOption (long "server" <> short 's' <> help "SMTP server address.")
  <*> encryptionSettings
  <*> switch (long "starttls" <> help "Use STARTTLS.")

encryptionSettings :: Parser Settings
encryptionSettings = Settings
  <$> option auto (long "ssl-port" <> short 's' <> help "SSL port.")
  <*> option auto (long "max-line-length" <> short 'l' <> help "Maximum line length.")
  <*> switch (long "log" <> help "Log to console.")
  <*> switch (long "disable-certificate-validation" <> help "Disable certificate validation.")

recipientParser :: Parser Address
recipientParser = strOption (long "to" <> help "Mail recipients.")

main :: IO ()
main = do
  CliOptions smtpServer recipients dryRun <- parseOptions

  message <- getContents <&> fromStrict <&> MsgPack.unpack
  case message of
    Just (Message feed element) -> do
      timezone <- io getCurrentTimeZone
      currentTime <- io getCurrentTime
      let formatMail = FormatMail defaultFormatFrom defaultFormatSubject defaultFormatBody (const $ const recipients)
          mail = buildMail formatMail currentTime timezone feed element
      unless dryRun $ io $ withSMTPConnection smtpServer $ sendMimeMail2 mail
    _ -> putStrLn "Invalid input" >> exitFailure

  return ()


-- * Default behavior

-- | Fill 'addressName' with the feed title and, if available, the authors' names.
--
-- This function leaves 'addressEmail' empty. You are expected to fill it adequately, because many SMTP servers enforce constraints on the From: email.
defaultFormatFrom :: Feed -> FeedElement -> Address
defaultFormatFrom (Rss doc) (RssElement item) = Address (Just $ channelTitle doc <> " (" <> itemAuthor item <> ")") ""
defaultFormatFrom (Atom feed) (AtomElement entry) = Address (Just $ title <> " (" <> authors <> ")") ""
  where title = show . prettyAtomText $ feedTitle feed
        authors = Text.intercalate ", " $ map (unrefine . personName) $ entryAuthors entry <> feedAuthors feed
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
defaultFormatBody _ (AtomElement entry) = "<p>" <> Text.intercalate "<br/>" links <> "</p><p>" <> fromMaybe "<empty>" (content <|> summary) <> "</p>"
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
withSMTPConnection (SMTPServer authentication (Encrypted server settings False)) f =
  doSMTPSSLWithSettings server settings $ \connection -> do
    forM_ authentication (authenticate_ connection)
    f connection
withSMTPConnection (SMTPServer authentication (Encrypted server settings True)) f =
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
