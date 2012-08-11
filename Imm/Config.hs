module Imm.Config where

-- {{{ Imports
import Imm.Feed
import Imm.Types
import Imm.Util

import Control.Monad.Error hiding(forM_)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.Default
import Data.Foldable hiding(concat)
import Data.Text.ICU.Convert
import qualified Data.Text.Lazy as TL

import System.Directory
import System.Environment.XDG.BaseDir

import Text.Feed.Query
-- }}}


instance Default Settings where
    def = Settings {
        mStateDirectory = getUserConfigDir "imm" >/> "state",
        mMaildir        = getHomeDirectory >/> "feeds",
        mFromBuilder    = \(item, feed) -> maybe (getFeedTitle feed) id $ getItemAuthor item,
        mSubjectBuilder = \(item, _feed) -> TL.pack . maybe "Untitled" id $ getItemTitle item,
        mBodyBuilder    = \(item, _feed) -> TL.unlines $ map (flip ($) item) [TL.pack . getItemLinkNM, getItemContent],
        mDecoder        = \raw -> catchError (decodeUtf8 raw) $ return $ do
            conv <- io $ open "ISO-8859-1" Nothing
            return . TL.fromChunks . (\a -> [a]) . toUnicode conv . B.concat . BL.toChunks $ raw
    }

-- | Return the Haskell code to write in the configuration file to add a feed.
addFeeds :: MonadIO m => [(String, [String])] -> m ()
addFeeds feeds = io . forM_ feeds $ \(groupTitle, uris) -> do
    putStrLn $ "-- Group " ++ groupTitle
    putStrLn $ map toLower (concat . words $ groupTitle) ++ " = ["
    forM_ uris (\uri -> putStrLn $ "    " ++ show uri ++ ",")
    putStrLn "]"
    putStrLn ""
    
