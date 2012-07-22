module Imm.Boot where

-- {{{ Imports
import qualified Imm.Core as Imm
import Imm.Types

import qualified Config.Dyre as D
import Config.Dyre.Paths

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
    &= verbosityArgs [explicit, name "Verbose", name "v"] []
    &= versionArg [ignore]
    &= help "Convert items from RSS/Atom feeds to maildir entries."
    &= helpArg [explicit, name "help", name "h"]
    &= program "imm"

-- | Main function to call.
imm :: Settings -> IO ()
imm settings = do
    options <- getOptions
    D.wrapMain dyreParameters (Right (settings, options))
-- }}}

-- {{{ Dynamic reconfiguration
printDyrePaths :: IO ()
printDyrePaths = getPaths dyreParameters >>= \(a, b, c, d, e) -> (putStrLn . unlines) [
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

-- Main dispatcher, depending on commandline options
realMain :: Either String (Settings, CliOptions) -> IO ()
realMain (Left e) = putStrLn e
realMain (Right (s, options))
  | mList  options = mapM_ Imm.printFeedGroup $ mFeedGroups s
  | mCheck options = mapM_ Imm.checkFeedGroup $ mFeedGroups s
  | otherwise      = do
    whenLoud printDyrePaths
    Imm.main (s, options)
-- }}}
