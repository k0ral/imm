{-# LANGUAGE TupleSections #-}
module Imm.Boot (imm, ConfigFeed) where

-- {{{ Imports
import qualified Imm.Core as Core
import Imm.Config
import Imm.Dyre as Dyre
import Imm.Options (CliOptions)
import qualified Imm.Options as Options
import Imm.Util

import Control.Conditional
import Control.Lens hiding ((??))

import Data.Default
import Data.Either
import Data.Maybe

import Network.URI as N

import System.Directory
import System.Exit
-- }}}

type ConfigFeed = (Config -> Config, String)

-- | Main function to call in the configuration file.
imm :: [ConfigFeed] -> IO ()
imm feedsFromConfig = do
    options <- Options.get

    when (view Options.help options) $ putStrLn Options.usage >> exitSuccess

    Dyre.wrap realMain options (options, feedsFromConfig)


validateFeeds :: [ConfigFeed] -> [URI] -> ([String], Core.FeedList)
validateFeeds feedsFromConfig feedsFromOptions = (errors ++ errors', null feedsFromOptions ? feedsOK ?? feedsOK')
  where
    validateFromConfig (x, u) = maybe (Left ("Invalid feed URI: " ++ u)) (Right . (x,)) $ N.parseURI u
    validateFromOptions uri   = maybe (Left ("URI from commandline option has no configuration entry: " ++ show uri)) Right . listToMaybe . (filter ((== uri) . snd)) $ feedsOK
    (errors,  feedsOK)        = partitionEithers $ map validateFromConfig  feedsFromConfig
    (errors', feedsOK')       = partitionEithers $ map validateFromOptions feedsFromOptions


realMain :: (CliOptions, [ConfigFeed]) -> IO ()
realMain (options, feedsFromConfig) = do
    let (errors, feedsOK) = validateFeeds feedsFromConfig (view Options.feedList options)
    when (not $ null errors) . putStrLn $ unlines errors

    when (null feedsOK) $ putStrLn "Nothing to process. Exiting..." >> exitFailure
    -- when (view Options.verbose options) . putStrLn . unlines $ map (show . snd) feedsOK

    home <- getHomeDirectory >/> "feeds"
    let config = set maildir home def
    dispatch feedsOK config options


dispatch :: Core.FeedList -> Config -> CliOptions -> IO ()
dispatch feeds config options
    | options^.Options.check               = Core.check options feeds
    | options^.Options.list                = Core.list options feeds
    | options^.Options.markAsRead          = Core.markAsRead options feeds
    | options^.Options.markAsUnread        = Core.markAsUnread options feeds
    | options^.Options.update              = Core.update options feeds
    | isJust (options^.Options.importOPML) = Core.importOPML
    | otherwise                            = putStrLn Options.usage
