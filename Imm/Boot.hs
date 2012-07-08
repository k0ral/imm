module Imm.Boot where

-- {{{ Imports
import Imm.Core
import Imm.Types

import qualified Config.Dyre as D
import Config.Dyre.Paths

import Data.Foldable

import System.Console.CmdArgs
import System.IO
-- }}}

-- {{{ Commandline options
-- | Available commandline options
cliOptions :: CliOptions
cliOptions = CliOptions {
    mParameter     = def &= help "option description" &= explicit &= name "p" &= name "parameter" &= typ "type of the argument",
    mMasterBinary  = def &= name "dyre-master-binary" &= explicit
}

getOptions :: IO CliOptions
getOptions = cmdArgs $ cliOptions
    &= verbosityArgs [explicit, name "Verbose", name "v"] []
    &= versionArg [ignore]
    &= help "Fetch and send items from RSS/Atom feeds to a custom mail address."
    &= helpArg [explicit, name "help", name "h"]
    &= program "imm"
-- }}}

-- {{{ Dynamic reconfiguration
printDyrePaths :: IO ()
printDyrePaths = getPaths (dyreParameters []) >>= \(a, b, c, d, e) -> (putStrLn . unlines) [
    "Current binary:  " ++ a,
    "Custom binary:   " ++ b,
    "Config file:     " ++ c,
    "Cache directory: " ++ d,
    "Lib directory:   " ++ e, []]

dyreParameters :: [FeedGroup] -> D.Params (Settings, CliOptions)
dyreParameters feedGroups = D.defaultParams {
  D.projectName  = "imm",
  D.showError    = showError,
  D.realMain     = realMain feedGroups,
  D.ghcOpts      = ["-threaded"],
  D.statusOut    = hPutStrLn stderr
}

showError :: (Settings, a) -> String -> (Settings, a)
showError (settings, x) message = (settings { mError = Just message }, x)
-- }}}

-- | 
imm :: [FeedGroup] -> Settings -> IO ()
imm feedGroups parameters = do
    forM_ (mError parameters) putStrLn
    whenLoud printDyrePaths
    
    options <- getOptions
    D.wrapMain (dyreParameters feedGroups) (parameters, options)
