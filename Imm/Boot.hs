-- | Low level functions used at start-up to parse commandline and handle dynamic reconfiguration.
module Imm.Boot where

-- {{{ Imports
import qualified Imm.Main as Main
import Imm.Types

import qualified Config.Dyre as D
import Config.Dyre.Paths

import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit
import System.IO
-- }}}

-- {{{ Commandline options
-- | Available commandline options.
cliOptions :: Mode (CmdArgs CliOptions)
cliOptions = cmdArgsMode $ baseOptions
    &= verbosityArgs [explicit, name "verbose", name "v"] []
    &= versionArg [ignore]
    &= help "Convert items from RSS/Atom feeds to maildir entries."
    &= helpArg [explicit, name "help", name "h"]
    &= program "imm"
  where
    baseOptions = CliOptions {
        --    mCheck         = def &= explicit &= name "c" &= name "check" &= help "Check availability and validity of all feed sources currently configured.",
            mFeedURI       = def &= explicit &= name "f" &= name "feed"   &= help "Only process given feed." &= typ "URI",
            mImportOPML    = def &= explicit &= name "i" &= name "import" &= help "Import feeds list from an OPML descriptor (read from stdin).",
            mList          = def &= explicit &= name "l" &= name "list"  &= help "List all feed sources currently configured, along with their status.",
            mMarkAsRead    = def &= explicit &= name "R" &= name "mark-read" &= help "Mark every item of processed feeds as read, ie set last update as now without writing any mail.",
            mMarkAsUnread  = def &= explicit &= name "U" &= name "mark-unread" &= help "Mark every item of processed feeds as unread, ie delete corresponding state files.",
            mUpdate        = def &= explicit &= name "u" &= name "update" &= help "Update list of feeds (mostly used option).",
            mDenyReconf    = def &= explicit &= name "deny-reconf"       &= help "Deny recompilation even if the configuration file has changed.",
            mMasterBinary  = def &= explicit &= name "dyre-master-binary" &= help "Flag used internally for dynamic reconfiguration purposes." &= typ "PATH"}
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
dyreParameters :: D.Params (Either String FeedList)
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
imm = D.wrapMain dyreParameters . Right

-- | Internal dispatcher, decides which function to execute depending on commandline options.
realMain :: Either String FeedList -> IO ()
realMain (Left e) = putStrLn e
realMain (Right feeds) = do
    options <- cmdArgsRun cliOptions
    whenLoud printDyrePaths
    let feeds' = case (mFeedURI options) of
          Just uri -> filter (\(_, u) -> u == uri) feeds
          _        -> feeds
    realMain' (feeds', options)
  where
    realMain' (f, options)
      | mImportOPML options   = Main.importOPML
      | mList       options   = Main.list f
      | mMarkAsRead options   = Main.markAsRead f
      | mMarkAsUnread options = Main.markAsUnread f
--      | mCheck options = Main.check f
      | mUpdate     options   = Main.update f
      | otherwise             = print $ helpText [] HelpFormatDefault cliOptions
-- }}}
