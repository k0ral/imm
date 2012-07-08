module Imm.Core where

-- {{{ Imports
import Imm.Mail
import qualified Imm.Maildir as Maildir
import Imm.Types
import Imm.Util

--import Control.Arrow
import Control.Exception
import Control.Monad hiding(forM_)

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as B
import Data.Foldable
--import Data.Functor
--import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX

import Network.HTTP hiding(Response)
import Network.URI

import System.Directory
import System.FilePath
import System.IO
import System.Locale

import qualified Text.Feed.Import as F
import Text.Feed.Query
import Text.Feed.Types
-- }}}

-- Entry point
realMain :: [FeedGroup] -> (Settings, CliOptions) -> IO ()
realMain feedGroups (parameters, options) = do        
    void . mapM (processFeedGroup parameters) $ feedGroups
   
processFeedGroup :: Settings -> FeedGroup -> IO ()
processFeedGroup parameters _feedGroup@(settings, feedURIs) = do    
    result <- Maildir.init . mMailDirectory $ settings
    
    when result $ do
      let uris   = map parseURI' feedURIs 
      rawFeeds  <- mapM (either (return . Left) downloadRaw) uris 
      let feeds  = zip feedURIs . map (parseFeedString =<<) $ rawFeeds
    
      void . mapM (processFeed parameters settings) $ feeds 
    
    return ()

parseURI' :: String -> Either String URI
parseURI' uri = maybe (Left . ("Ill-formatted URI: " ++) $ uri) (Right) . parseURI $ uri

processFeed :: Settings -> FeedSettings -> (String, Either String Feed) -> IO ()
processFeed _ _ (_, Left e) = putStrLn e
processFeed parameters settings (uri, Right feed) = do
    logVerbose $ unlines [
        "Processing feed: " ++ uri,
        ("Title:  " ++) . getFeedTitle $ feed,
        ("Author: " ++) . maybe "No author" id . getFeedAuthor $ feed,
        ("Home:   " ++) . maybe "No home"   id . getFeedHome $ feed]
    
    directory <- resolve $ mStateDirectory parameters
    let fileName  = uri >>= escapeFileName
    
-- 
    oldTime <- try $ readFile (directory </> fileName) :: IO (Either IOError String)
    let timeZero = posixSecondsToUTCTime $ 0 
    let threshold = either
          (const timeZero)
          (maybe timeZero id . parseTime defaultTimeLocale "%F %T %Z")
          oldTime
    
    lastTime <- foldlM (\acc item -> processItem parameters settings threshold item >>= (return . (max acc))) threshold (feedItems feed) 
    
-- 
    (file, stream) <- openTempFile directory fileName
    hPutStrLn stream (show lastTime)
    hClose stream
    renameFile file (directory </> fileName)
    
    return ()

processItem :: Settings -> FeedSettings -> UTCTime -> Item -> IO UTCTime
processItem parameters settings threshold item = do
--  currentTime <- getCurrentTime :: IO UTCTime
    timeZone    <- getCurrentTimeZone
  
    logVerbose $ unlines ["",
        "   Item author: " ++ (maybe "" id $ getItemAuthor item),
        "   Item title:  " ++ (maybe "" id $ getItemTitle item),
        "   Item URI:    " ++ (maybe "" id $ getItemLink  item),
        "   Item Content:    " ++ (Imm.Mail.getItemContent  item),
        "   Item date:   " ++ (maybe "" id $ time)]
    
    case time >>= parseDate of
        Just y -> do
            when (threshold < y) $ do
                logVerbose "==> New entry added to maildir."
                Maildir.add dir . itemToMail timeZone $ item 
            return y
        _      -> do
            Maildir.add dir . itemToMail timeZone $ item 
            return threshold
  where
    time = getItemDate item
    dir  = mMailDirectory settings

downloadRaw :: URI -> IO (Either String String)
downloadRaw uri = do
    result <- simpleHTTP (mkRequest GET uri :: Request B.ByteString)
    return . either (Left . show) (Right . B.toString . rspBody) $ result

-- | Same as Text.Feed.Import.ParseFeedString, but with Either monad.
parseFeedString :: String -> Either String Feed
parseFeedString = maybe
    (Left "Unable to parse XML from raw page.") 
    Right 
    . F.parseFeedString
