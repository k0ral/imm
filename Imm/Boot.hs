module Imm.Boot where

-- {{{ Imports
import Imm.Main
import Imm.Types

import qualified Config.Dyre as D
import Config.Dyre.Paths

import Control.Monad.Reader

import System.Console.CmdArgs
import System.IO
-- }}}

-- {{{ Commandline options
-- | Available commandline options
cliOptions :: CliOptions
cliOptions = CliOptions {
    mCheck         = def &= explicit &= name "c" &= name "check" &= help "Check availability and validity of all feed sources currently configured.",
    mList          = def &= explicit &= name "l" &= name "list"  &= help "List all feed sources currently configured.",
    mDenyReconf    = def &= explicit &= name "deny-reconf"       &= help "Deny recompilation even if the configuration file has changed.",
    mMasterBinary  = def &= explicit &= name "dyre-master-binary"
}

getOptions :: IO CliOptions
getOptions = cmdArgs $ cliOptions
    &= verbosityArgs [explicit, name "verbose", name "v"] []
    &= versionArg [ignore]
    &= help "Convert items from RSS/Atom feeds to maildir entries."
    &= helpArg [explicit, name "help", name "h"]
    &= program "imm"
-- }}}

-- {{{ Dynamic reconfiguration
printDyrePaths :: IO ()
printDyrePaths = do
    (a, b, c, d, e) <- getPaths dyreParameters
    putStrLn . unlines $ [
        "Current binary:  " ++ a,
        "Custom binary:   " ++ b,
        "Config file:     " ++ c,
        "Cache directory: " ++ d,
        "Lib directory:   " ++ e, []]

dyreParameters :: D.Params (Either String (Settings, CliOptions))
dyreParameters = D.defaultParams {
  D.projectName  = "imm",
  D.showError    = showError,
  D.realMain     = realMain,
  D.ghcOpts      = ["-threaded"],
  D.statusOut    = hPutStrLn stderr
}

showError :: Either String a -> String -> Either String a
showError _ = Left
-- }}}

-- | Main function to call.
imm :: Settings -> IO ()
imm settings = do
    options <- getOptions
    D.wrapMain dyreParameters (Right (settings, options))

-- Main dispatcher, depending on commandline options
realMain :: Either String (Settings, CliOptions) -> IO ()
realMain (Left e) = putStrLn e
realMain (Right (settings, options))
  | mList  options = mapM_ (flip runReaderT settings . printFeedGroupStatus) $ mFeedGroups settings
  | mCheck options = mapM_ (flip runReaderT settings . checkFeedGroup) $ mFeedGroups settings
  | otherwise      = whenLoud printDyrePaths >> runReaderT main settings
-- }}}
