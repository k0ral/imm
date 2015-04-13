{-# LANGUAGE TupleSections #-}
module Imm.Boot (imm, ConfigFeed) where

-- {{{ Imports
import qualified Imm.Core as Core
import Imm.Config
import Imm.Database
import Imm.Dyre as Dyre
import qualified Imm.Feed as Feed
import Imm.Options (Action(..), OptionsReader(..))
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
imm feedsFromConfig = Options.run $ readOptions Options.action >>= dispatch feedsFromConfig


dispatch :: [ConfigFeed] -> Options.Action -> ReaderT Options.CliOptions IO ()
dispatch _ Help        = io $ putStrLn Options.usage >> exitSuccess
dispatch _ ShowVersion = io $ putStrLn (showVersion version) >> exitSuccess
dispatch _ Recompile   = io $ Dyre.recompile >>= maybe exitSuccess (\e -> putStrLn e >> exitFailure)
dispatch _ Import      = io getContents >>= Core.importOPML >> io exitSuccess
dispatch feedsFromConfig (Run action) = do
    dyreMode         <- readOptions Options.dyreMode
    feedsFromOptions <- readOptions Options.feedsList
    dataDir          <- readOptions Options.dataDirectory

    io $ Dyre.wrap dyreMode realMain (action, dataDir, feedsFromOptions, feedsFromConfig)

validateFeeds :: [ConfigFeed] -> [URI] -> ([String], Core.FeedList)
validateFeeds feedsFromConfig feedsFromOptions = (errors ++ errors', null feedsFromOptions ? feedsOK ?? feedsOK')
  where
    validateFromConfig (x, u) = maybe (Left ("Invalid feed URI: " ++ u)) (Right . (x,)) $ N.parseURI u
    validateFromOptions uri   = maybe (Left ("URI from commandline option has no configuration entry: " ++ show uri)) Right . listToMaybe . filter ((== uri) . snd) $ feedsOK
    (errors,  feedsOK)        = partitionEithers $ map validateFromConfig  feedsFromConfig
    (errors', feedsOK')       = partitionEithers $ map validateFromOptions feedsFromOptions


realMain :: (Feed.Action, Maybe FilePath, [URI], [ConfigFeed]) -> IO ()
realMain (action, dataDir, feedsFromOptions, feedsFromConfig) = do
    unless (null errors)  . errorM   "imm.boot" $ unlines errors
    when   (null feedsOK) $ warningM "imm.boot"   "Nothing to process. Exiting..." >> exitFailure
    -- io . debugM "imm.boot" . unlines $ "Feeds to be processed:":(map (show . snd) feedsOK)

    Core.dispatch baseConfig action feedsOK
  where
    (errors, feedsOK) = validateFeeds feedsFromConfig feedsFromOptions
    baseConfig        = maybe id (set (fileDatabase . directory)) dataDir
