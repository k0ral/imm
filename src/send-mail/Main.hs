{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- 'Callback' for @imm@ that sends a mail via a SMTP server the input RSS/Atom item.
--  {{{ Imports

import Data.Aeson
import Data.ByteString.Lazy (getContents)
import Data.Text as Text (intercalate)
import Data.Time
import Dhall (FromDhall (..), auto, input)
import Imm.Callback (CallbackMessage (..))
import Imm.Feed
import Imm.Link
import Imm.Pretty
import Network.Mail.Mime hiding (sendmail)
import Options.Applicative hiding (auto)
import System.Directory (XdgDirectory (..), getXdgDirectory)
import System.FilePath
import System.Process.Typed
import URI.ByteString.Extended

-- }}}

-- | How to call external command
data Command = Command
  { _executable :: FilePath,
    _arguments :: [Text]
  }
  deriving (Eq, Generic, Ord, Read, Show)

instance FromDhall Command

-- | How to format outgoing mails from feed elements
data FormatMail = FormatMail
  { -- | How to write the From: header of feed mails
    formatFrom :: FeedDefinition -> FeedItem -> Address,
    -- | How to write the Subject: header of feed mails
    formatSubject :: FeedDefinition -> FeedItem -> Text,
    -- | How to write the body of feed mails (sic!)
    formatBody :: FeedDefinition -> FeedItem -> Text,
    -- | How to write the To: header of feed mails
    formatTo :: FeedDefinition -> FeedItem -> [Address]
  }

data CliOptions = CliOptions
  { _configFile :: FilePath,
    _recipients :: [Address],
    _dryRun :: Bool
  }
  deriving (Eq, Generic, Show)

parseOptions :: MonadIO m => m CliOptions
parseOptions = io $ do
  defaultConfigFile <- getXdgDirectory XdgConfig $ "imm" </> "sendmail.dhall"
  customExecParser (prefs $ showHelpOnError <> showHelpOnEmpty) (info (cliOptions defaultConfigFile <**> helper) $ progDesc description)

description :: String
description = "Send a mail for each new RSS/Atom item."

cliOptions :: FilePath -> Parser CliOptions
cliOptions defaultConfigFile =
  CliOptions
    <$> (configFileOption <|> pure defaultConfigFile)
    <*> many recipientParser
    <*> switch (long "dry-run" <> help "Disable all I/Os, except for logs.")

configFileOption :: Parser FilePath
configFileOption = strOption $ long "config" <> short 'c' <> metavar "FILE" <> help "Dhall configuration file for SMTP client call"

recipientParser :: Parser Address
recipientParser = strOption (long "to" <> help "Mail recipients.")

main :: IO ()
main = do
  CliOptions configFile recipients dryRun <- parseOptions
  Command executable arguments <- input auto $ fromString configFile

  message <- getContents <&> eitherDecode
  case message of
    Right (CallbackMessage _ feed element) -> do
      timezone <- io getCurrentTimeZone
      currentTime <- io getCurrentTime
      let formatMail = FormatMail defaultFormatFrom defaultFormatSubject defaultFormatBody (const $ const recipients)
          mail = buildMail formatMail currentTime timezone feed element

      unless dryRun $ do
        processInput <- renderMail' mail <&> byteStringInput
        let processConfig = proc executable (toString <$> arguments) & setStdin processInput

        (exitCode, _output, errors) <- readProcess processConfig
        case exitCode of
          ExitSuccess -> exitSuccess
          ExitFailure _ -> putStrLn (decodeUtf8 errors) >> exitFailure
    Left e -> putStrLn ("Invalid input: " <> e) >> exitFailure

  return ()

-- * Default behavior

-- | Fill 'addressName' with the feed title and, if available, the authors' names.
--
-- This function leaves 'addressEmail' empty. You are expected to fill it adequately, because many SMTP servers enforce constraints on the From: email.
defaultFormatFrom :: FeedDefinition -> FeedItem -> Address
defaultFormatFrom feed item = Address (Just $ title <> " (" <> authors <> ")") ""
  where
    title = _feedTitle feed
    authors = Text.intercalate ", " $ map _authorName $ _itemAuthors item

-- | Fill mail subject with the element title
defaultFormatSubject :: FeedDefinition -> FeedItem -> Text
defaultFormatSubject _ = _itemTitle

-- | Fill mail body with:
--
-- - a list of links associated to the element
-- - the element's content or description/summary
defaultFormatBody :: FeedDefinition -> FeedItem -> Text
defaultFormatBody _ item = "<p>" <> Text.intercalate "<br/>" links <> "</p><p>" <> content <> "</p>"
  where
    links = map (withAnyURI (show . prettyURI) . _linkURI) $ _itemLinks item
    content = _itemContent item

-- * Low-level helpers

-- | Build mail from a given feed
buildMail :: FormatMail -> UTCTime -> TimeZone -> FeedDefinition -> FeedItem -> Mail
buildMail format currentTime timeZone feed element =
  let date = formatTime defaultTimeLocale "%a, %e %b %Y %T %z" $ utcToZonedTime timeZone $ fromMaybe currentTime $ _itemDate element
   in Mail
        { mailFrom = formatFrom format feed element,
          mailTo = formatTo format feed element,
          mailCc = [],
          mailBcc = [],
          mailHeaders =
            [ ("Return-Path", "<imm@noreply>"),
              ("Date", fromString date),
              ("Subject", formatSubject format feed element),
              ("Content-disposition", "inline")
            ],
          mailParts = [[htmlPart $ fromStrict $ formatBody format feed element]]
        }
