{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

-- Save page related to input RSS/Atom item, into shiori bookmark manager.
--
--  Meant to be used as a callback for imm.
--  {{{ Imports

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (getContents)
import Imm.Callback
import Imm.Feed
import Imm.Link
import Imm.Pretty
import Options.Applicative (Parser, execParser, forwardOptions, help, helper, info, long, progDesc, short, strOption, switch)
import System.Process.Typed
import URI.ByteString.Extended

-- }}}

data CliOptions = CliOptions
  { _tags ∷ String
  , _dryRun ∷ Bool
  }
  deriving (Eq, Ord, Read, Show)

parseOptions ∷ MonadIO m ⇒ m CliOptions
parseOptions = io $ execParser $ info (cliOptions <**> helper) $ progDesc description <> forwardOptions
 where
  description = "Save page into shiori bookmark manager, for each new RSS/Atom item."

cliOptions ∷ Parser CliOptions
cliOptions =
  CliOptions
    <$> strOption (long "tags" <> short 't' <> help "Comma-separated tags for this bookmark")
    <*> switch (long "dry-run" <> help "Disable all I/Os, except for logs.")

main ∷ IO ()
main = do
  CliOptions tags dryRun ← parseOptions
  input ← getContents <&> eitherDecode

  case input ∷ Either String CallbackMessage of
    Right (CallbackMessage _feedLocation _feedDefinition item) → do
      unless dryRun $ do
        case getMainLink item of
          Just link → saveShiori tags (_linkURI link) >>= exitWith
          _ → putStrLn ("No main link in item " <> show (prettyName item)) >> exitFailure
    Left e → putStrLn ("Invalid input: " <> e) >> exitFailure
  return ()

saveShiori ∷ String → AnyURI → IO ExitCode
saveShiori tags uri =
  runProcess $
    proc "shiori" ["add", show $ pretty uri, "-t", tags]
      & setStdin nullStream
      & setStdout inherit
      & setStderr inherit
