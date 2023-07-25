{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

-- Save page related to input RSS/Atom item, into a Wallabag server.
--
--  Meant to be used as a callback for imm.
--  {{{ Imports

import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecode, object, (.=))
import Data.ByteString.Lazy (getContents)
import Imm.Callback
import Imm.Feed
import Imm.Link
import Imm.Pretty
import Network.HTTP.Req
import Options.Applicative (Parser, auto, execParser, forwardOptions, help, helper, info, long, option, progDesc, short, strOption, switch)
import URI.ByteString.Extended

-- }}}

data CliOptions = CliOptions
  { _hostname ∷ Text
  , _port ∷ Int
  , _clientID ∷ String
  , _clientSecret ∷ String
  , _username ∷ String
  , _password ∷ String
  , _dryRun ∷ Bool
  }
  deriving (Eq, Ord, Read, Show)

parseOptions ∷ MonadIO m ⇒ m CliOptions
parseOptions = io $ execParser $ info (cliOptions <**> helper) $ progDesc description <> forwardOptions
 where
  description = "Save page into a Wallabag server, for each new RSS/Atom item."

cliOptions ∷ Parser CliOptions
cliOptions =
  CliOptions
    <$> strOption (long "host" <> short 'H' <> help "Hostname of the Wallabag server.")
    <*> option auto (long "port" <> short 'P' <> help "Port of the Wallabag server.")
    <*> strOption (long "client-id" <> short 'i' <> help "Client ID used to access the Wallabag API.")
    <*> strOption (long "client-secret" <> short 's' <> help "Client secret used to access the Wallabag API.")
    <*> strOption (long "username" <> short 'u' <> help "Username to log into Wallabag.")
    <*> strOption (long "password" <> short 'p' <> help "Password to log into Wallabag.")
    <*> switch (long "dry-run" <> help "Disable all I/Os, except for logs.")

main ∷ IO ()
main = do
  CliOptions wallabagHost wallabagPort clientID clientSecret username password dryRun ← parseOptions
  input ← getContents <&> eitherDecode

  case input ∷ Either String CallbackMessage of
    Right (CallbackMessage _feedLocation _feedDefinition item) → do
      unless dryRun $ do
        case getMainLink item of
          Just link → do
            runReq defaultHttpConfig $ do
              oAuthToken ← retrieveOAuthToken wallabagHost wallabagPort clientID clientSecret username password
              saveWallabag wallabagHost wallabagPort (access_token oAuthToken) $ _linkURI link
          _ → putStrLn ("No main link in item " <> show (prettyName item)) >> exitFailure
    Left e → putStrLn ("Invalid input: " <> e) >> exitFailure
  return ()

data OAuthTokenResponse = OAuthTokenResponse
  { access_token ∷ Text
  , expires_in ∷ Int
  , refresh_token ∷ Text
  , token_type ∷ Text
  }
  deriving (Generic, Show)

instance ToJSON OAuthTokenResponse

instance FromJSON OAuthTokenResponse

retrieveOAuthToken ∷ MonadHttp m ⇒ Text → Int → String → String → String → String → m OAuthTokenResponse
retrieveOAuthToken wallabagHost wallabagPort clientID clientSecret username password = do
  let payload =
        object
          [ "grant_type" .= ("password" ∷ String)
          , "client_id" .= clientID
          , "client_secret" .= clientSecret
          , "username" .= username
          , "password" .= password
          ]

  responseBody <$> req POST (http wallabagHost /: "oauth" /: "v2" /: "token") (ReqBodyJson payload) jsonResponse (port wallabagPort)

saveWallabag ∷ MonadHttp m ⇒ Text → Int → Text → AnyURI → m ()
saveWallabag wallabagHost wallabagPort accessToken uri = do
  let payload = object ["url" .= (decodeUtf8 $ withAnyURI serializeURIRef' uri ∷ Text)]
      options = port wallabagPort <> header "Authorization" ("Bearer " <> encodeUtf8 accessToken)
  responseBody <$> req POST (http wallabagHost /: "api" /: "entries.json") (ReqBodyJson payload) jsonResponse options
