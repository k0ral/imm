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
    mList          = def &= help "List all feed sources currently configured." &= explicit &= name "l" &= name "list",
    mMasterBinary  = def &= name "dyre-master-binary" &= explicit
}

getOptions :: IO CliOptions
getOptions = cmdArgs $ cliOptions
    &= verbosityArgs [explicit, name "Verbose", name "v"] []
    &= versionArg [ignore]
    &= help "Convert items from RSS/Atom feeds maildir entries."
    &= helpArg [explicit, name "help", name "h"]
    &= program "imm"

-- | Main function to call.
imm :: [FeedGroup] -> Settings -> IO ()
imm feedGroups settings = do
    options <- getOptions
    D.wrapMain (dyreParameters feedGroups) (Right (settings, options))
-- }}}

-- {{{ Dynamic reconfiguration
printDyrePaths :: IO ()
printDyrePaths = getPaths (dyreParameters []) >>= \(a, b, c, d, e) -> (putStrLn . unlines) [
    "Current binary:  " ++ a,
    "Custom binary:   " ++ b,
    "Config file:     " ++ c,
    "Cache directory: " ++ d,
    "Lib directory:   " ++ e, []]

dyreParameters :: [FeedGroup] -> D.Params (Either String (Settings, CliOptions))
dyreParameters feedGroups = D.defaultParams {
  D.projectName  = "imm",
  D.showError    = showError,
  D.realMain     = realMain feedGroups,
  D.ghcOpts      = ["-threaded"],
  D.statusOut    = hPutStrLn stderr
}

showError :: Either String a -> String -> Either String a
showError _ = Left

realMain :: [FeedGroup] -> Either String (Settings, CliOptions) -> IO ()
realMain _ (Left e) = putStrLn e
realMain feedGroups (Right s) = do
    whenLoud printDyrePaths
    Imm.main feedGroups s
-- }}}
