-- | Low level functions used at start-up to parse commandline and handle dynamic reconfiguration.
module Imm.Boot where

-- {{{ Imports
import Imm.Config
import Imm.Feed
import Imm.Main
import Imm.Types
import Imm.Util

import qualified Config.Dyre as D
import Config.Dyre.Paths

import Control.Monad.Error
import Control.Monad.Reader

import System.Console.CmdArgs
import System.IO
-- }}}

-- {{{ Commandline options
-- | Available commandline options.
cliOptions :: CliOptions
cliOptions = CliOptions {
    mCheck         = def &= explicit &= name "c" &= name "check" &= help "Check availability and validity of all feed sources currently configured.",
    mList          = def &= explicit &= name "l" &= name "list"  &= help "List all feed sources currently configured, along with their status.",
    mMarkAsRead    = def &= explicit &= name "R" &= name "mark-read" &= help "Mark every item of processed feeds as read, ie set last update as now without writing any mail.",
    mMarkAsUnread  = def &= explicit &= name "U" &= name "mark-unread" &= help "Mark every item of processed feeds as unread, ie delete corresponding state files.",
    mDenyReconf    = def &= explicit &= name "deny-reconf"       &= help "Deny recompilation even if the configuration file has changed.",
    mMasterBinary  = def &= explicit &= name "dyre-master-binary" &= help "Flag used internally for dynamic reconfiguration purposes."
}

-- | Retrieve and parse commandline options.
getOptions :: IO CliOptions
getOptions = cmdArgs $ cliOptions
    &= verbosityArgs [explicit, name "verbose", name "v"] []
    &= versionArg [ignore]
    &= help "Convert items from RSS/Atom feeds to maildir entries."
    &= helpArg [explicit, name "help", name "h"]
    &= program "imm"
-- }}}

-- {{{ Dynamic reconfiguration
-- | Print various paths used for dynamic reconfiguration.
printDyrePaths :: IO ()
printDyrePaths = do
    (a, b, c, d, e) <- getPaths dyreParameters
    putStrLn . unlines $ [
        "Current binary:  " ++ a,
        "Custom binary:   " ++ b,
        "Config file:     " ++ c,
        "Cache directory: " ++ d,
        "Lib directory:   " ++ e, []]

-- | Dynamic configuration settings.
dyreParameters :: D.Params (Either String (FeedList, CliOptions))
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

-- | Main function to call in your configuration file.
imm :: FeedList -> IO ()
imm feeds = do
    options <- getOptions
    D.wrapMain dyreParameters (Right (feeds, options))

-- | Internal dispatcher, decides which function to execute depending on commandline options.
realMain :: Either String (FeedList, CliOptions) -> IO ()
realMain (Left e) = putStrLn e
realMain (Right (feeds, options))
  | mList  options = mapM_ (\(f, u) -> runReaderT (printStatus u) (f defaultSettings)) feeds
  | mMarkAsRead options = mapM_ (\(f,u) -> runReaderT (runErrorT $ checkStateDirectory >> parseURI u >>= markAsRead) (f defaultSettings)) feeds
  | mMarkAsUnread options = mapM_ (\(f,u) -> runReaderT (runErrorT $ parseURI u >>= markAsUnread) (f defaultSettings)) feeds
--  | mCheck options = mapM_ (flip runReaderT settings . checkFeedGroup) $ mFeedGroups settings
  | otherwise      = whenLoud printDyrePaths >> runErrorT (main feeds) >>= either print return
-- }}}
