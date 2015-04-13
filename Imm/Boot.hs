{-# LANGUAGE TupleSections #-}
module Imm.Boot (imm, ConfigFeed) where

-- {{{ Imports
import qualified Imm.Core as Core
import Imm.Config
import Imm.Database
import Imm.Dyre as Dyre
import Imm.Error
import Imm.Options (Action(..), Configuration(..), OptionsReader(..))
import qualified Imm.Options as Options
import Imm.Util

import Control.Lens hiding (Action, (??))
import Control.Monad.Error hiding(when)
import Control.Monad.Reader hiding(when)

import Data.Version

import Network.URI as N

import Paths_imm

import System.Log.Logger
import System.Exit
-- }}}

type ConfigFeed = (Config -> Config, String)


-- | Main function to call in the configuration file.
imm :: [ConfigFeed] -> IO ()
imm feedsFromConfig = Options.run $ do
    action           <- readOptions Options.action
    configuration    <- readOptions Options.configuration
    feedsFromOptions <- readOptions Options.feedsList
    dataDir          <- readOptions Options.dataDirectory

    when (action == Help)        . io $ putStrLn Options.usage >> exitSuccess
    when (action == ShowVersion) . io $ putStrLn (showVersion version) >> exitSuccess
    when (action == Recompile)   . io $ Dyre.recompile >>= maybe exitSuccess (\e -> putStrLn e >> exitFailure)

    io $ Dyre.wrap (configuration == Vanilla) realMain (action, dataDir, feedsFromOptions, feedsFromConfig)


validateFeeds :: [ConfigFeed] -> [URI] -> ([String], Core.FeedList)
validateFeeds feedsFromConfig feedsFromOptions = (errors ++ errors', null feedsFromOptions ? feedsOK ?? feedsOK')
  where
    validateFromConfig (x, u) = maybe (Left ("Invalid feed URI: " ++ u)) (Right . (x,)) $ N.parseURI u
    validateFromOptions uri   = maybe (Left ("URI from commandline option has no configuration entry: " ++ show uri)) Right . listToMaybe . filter ((== uri) . snd) $ feedsOK
    (errors,  feedsOK)        = partitionEithers $ map validateFromConfig  feedsFromConfig
    (errors', feedsOK')       = partitionEithers $ map validateFromOptions feedsFromOptions


realMain :: (Action, Maybe FilePath, [URI], [ConfigFeed]) -> IO ()
realMain (action, dataDir, feedsFromOptions, feedsFromConfig) = do
    let (errors, feedsOK) = validateFeeds feedsFromConfig feedsFromOptions
    unless (null errors) . errorM "imm.boot" $ unlines errors

    when (null feedsOK) $ warningM "imm.boot" "Nothing to process. Exiting..." >> exitFailure
    -- io . debugM "imm.boot" . unlines $ "Feeds to be processed:":(map (show . snd) feedsOK)

    withError . withConfig (maybe id (set (fileDatabase . directory)) dataDir) $ dispatch action feedsOK


dispatch :: Action -> Core.FeedList -> ReaderT Config (ErrorT ImmError IO) ()
dispatch CheckFeeds   feeds = mapM_ Core.check feeds
dispatch ListFeeds    feeds = mapM_ Core.list feeds
dispatch MarkAsRead   feeds = mapM_ Core.markAsRead feeds
dispatch MarkAsUnread feeds = mapM_ Core.markAsUnread feeds
dispatch UpdateFeeds  feeds = mapM_ Core.update feeds
dispatch ImportFeeds  _     = Core.importOPML =<< io getContents
dispatch _            _     = io $ putStrLn Options.usage
