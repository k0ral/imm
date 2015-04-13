{-# LANGUAGE TupleSections #-}
module Imm.Boot (imm, ConfigFeed) where

-- {{{ Imports
import qualified Imm.Core as Core
import Imm.Config
import Imm.Database
import Imm.Dyre as Dyre
import qualified Imm.Feed as Feed
import Imm.Options (Action(..))
import qualified Imm.Options as Options
import Imm.Util

import Control.Lens hiding (Action, (??))
import Control.Monad.Error hiding(mapM_, when)
-- import Control.Monad.Reader hiding(mapM_, when)
import Control.Monad.Trans.Maybe

import Data.Foldable
import Data.Version

import Network.URI as N

import Paths_imm
import Prelude hiding (mapM_)

import System.Log.Logger
import System.Exit
-- }}}

type ConfigFeed = (Config -> Config, String)


-- | Main function to call in the configuration file.
imm :: [ConfigFeed] -> IO ()
imm feedsFromConfig = void . runMaybeT $ do
    options <- Options.get
    let dataDir          = view Options.dataDirectory options
        dyreMode         = view Options.dyreMode      options
        feedsFromOptions = view Options.feedsList     options
        logLevel         = view Options.logLevel      options

    action <- handleSpecialActions $ view Options.action        options

    io . updateGlobalLogger rootLoggerName $ setLevel logLevel
    io . debugM "imm.options" $ "Commandline options: " ++ show options

    io $ Dyre.wrap dyreMode realMain (action, dataDir, feedsFromOptions, feedsFromConfig)


handleSpecialActions :: Options.Action -> MaybeT IO Feed.Action
handleSpecialActions Help         = (io $ putStrLn Options.usage) >> mzero
handleSpecialActions ShowVersion  = (io . putStrLn $ showVersion version) >> mzero
handleSpecialActions Recompile    = (io $ Dyre.recompile >>= mapM_ putStrLn) >> mzero
handleSpecialActions Import       = io getContents >>= Core.importOPML >> mzero
handleSpecialActions (Run action) = return action


realMain :: (Feed.Action, Maybe FilePath, [URI], [ConfigFeed]) -> IO ()
realMain (action, dataDir, feedsFromOptions, feedsFromConfig) = do
    unless (null errors)  . errorM   "imm.boot" $ unlines errors
    when   (null feedsOK) $ warningM "imm.boot"   "Nothing to process. Exiting..." >> exitFailure
    -- io . debugM "imm.boot" . unlines $ "Feeds to be processed:":(map (show . snd) feedsOK)

    Core.dispatch baseConfig action feedsOK
  where
    (errors, feedsOK) = validateFeeds feedsFromConfig feedsFromOptions
    baseConfig        = maybe id (set (fileDatabase . directory)) dataDir


validateFeeds :: [ConfigFeed] -> [URI] -> ([String], Core.FeedList)
validateFeeds feedsFromConfig feedsFromOptions = (errors ++ errors', null feedsFromOptions ? feedsOK ?? feedsOK')
  where
    validateFromConfig (x, u) = maybe (Left ("Invalid feed URI: " ++ u)) (Right . (x,)) $ N.parseURI u
    validateFromOptions uri   = maybe (Left ("URI from commandline option has no configuration entry: " ++ show uri)) Right . listToMaybe . filter ((== uri) . snd) $ feedsOK
    (errors,  feedsOK)        = partitionEithers $ map validateFromConfig  feedsFromConfig
    (errors', feedsOK')       = partitionEithers $ map validateFromOptions feedsFromOptions
