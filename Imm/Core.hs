module Imm.Core where

-- {{{ Imports
import Imm.Config
import Imm.Mail
import qualified Imm.Maildir as Maildir
import Imm.Types
import Imm.Util

import qualified Config.Dyre as D
import Config.Dyre.Paths

--import Control.Arrow
import Control.Monad hiding(forM_)

import Data.Foldable
--import Data.Functor
--import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX

import Network.HTTP hiding(Response)
import Network.URI

import System.Console.CmdArgs
import System.Directory
import System.FilePath
import System.IO
import System.IO.Error
import System.Locale

import qualified Text.Feed.Import as F
import Text.Feed.Query
import Text.Feed.Types
-- }}}

-- {{{ Commandline options                                                                             
-- | Available commandline options
cliOptions :: CliOptions
cliOptions = CliOptions {
    mParameter = def &= help "option description" &= explicit &= name "p" &= name "parameter" &= typ "type of the argument"
}

getOptions :: IO CliOptions
getOptions = cmdArgs $ cliOptions
    &= verbosityArgs [explicit, name "Verbose", name "v"] []
    &= versionArg [ignore]
    &= help "Fetch and send items from RSS/Atom feeds to a custom mail address."
    &= helpArg [explicit, name "help", name "h"]
    &= program "imm"
-- }}}

-- {{{ Configuration
dyreParameters :: D.Params Parameters
dyreParameters = D.defaultParams {
  D.projectName  = "imm",
  D.showError    = showError,
  D.realMain     = realMain,
  D.ghcOpts      = ["-threaded"],
  D.statusOut    = hPutStrLn stderr
}

showError :: Parameters -> String -> Parameters
showError parameters message = parameters { mError = Just message }
-- }}}

-- | 
imm :: Parameters -> IO ()
imm = D.wrapMain dyreParameters

-- Entry point
realMain :: Parameters -> IO ()
realMain parameters@Parameters{ mMailDirectory = directory } = do
-- Print configuration error, if any
    forM_ (mError parameters) putStrLn
    
-- Parse commandline arguments
    options <- getOptions

-- Print in-use paths
    (a, b, c, d, e) <- getPaths dyreParameters 
    whenLoud . putStrLn . unlines $ [
        "Current binary:  " ++ a,
        "Custom binary:   " ++ b,
        "Config file:     " ++ c,
        "Cache directory: " ++ d,
        "Lib directory:   " ++ e]
        
-- Initialize mailbox
    result <- Maildir.init directory
    when result $ realMain' parameters
   
-- At this point, a maildir has been setup.
realMain' :: Parameters -> IO ()
realMain' parameters@Parameters{ mFeedURIs = feedURIs } = do    
    let uris   = map parseURI' feedURIs 
    rawFeeds  <- mapM (either (return . Left) downloadRaw) uris 
    let feeds  = zip feedURIs . map (parseFeedString =<<) $ rawFeeds
    
    void . mapM (processFeed parameters) $ feeds 
    return ()

parseURI' :: String -> Either String URI
parseURI' uri = maybe (Left . ("Ill-formatted URI: " ++) $ uri) (Right) . parseURI $ uri

processFeed :: Parameters -> (String, Either String Feed) -> IO ()
processFeed _ (_, Left e) = putStrLn e
processFeed parameters (uri, Right feed) = do
    whenLoud . putStr . unlines $ [
        "Processing feed: " ++ uri,
        ("Title:  " ++) . getFeedTitle $ feed,
        ("Author: " ++) . maybe "No author" id . getFeedAuthor $ feed,
        ("Home:   " ++) . maybe "No home"   id . getFeedHome $ feed]
    
    (_, _, _, d, _) <- getPaths dyreParameters
    let directory = maybe d id . mCacheDirectory $ parameters  
    let fileName  = uri >>= escapeFileName
    
-- 
    oldTime <- try $ readFile (directory </> fileName)
    let timeZero = posixSecondsToUTCTime $ 0 
    let threshold = either
          (const timeZero)
          (maybe timeZero id . parseTime defaultTimeLocale "%F %T %Z")
          oldTime
    
    lastTime <- foldlM (\acc item -> processItem parameters threshold item >>= (return . (max acc))) threshold (feedItems feed) 
    
-- 
    (file, handle) <- openTempFile directory fileName
    hPutStrLn handle (show lastTime)
    hClose handle
    renameFile file (directory </> fileName)
    
    return ()

processItem :: Parameters -> UTCTime -> Item -> IO UTCTime
processItem parameters@Parameters{ mMailDirectory = directory } threshold item = do
    currentTime <- getCurrentTime :: IO UTCTime
    timeZone    <- getCurrentTimeZone
    let time = getItemDate item
  
    whenLoud . putStr . unlines $ ["",
        "   Item author: " ++ (maybe "" id $ getItemAuthor item),
        "   Item title:  " ++ (maybe "" id $ getItemTitle item),
        "   Item URI:    " ++ (maybe "" id $ getItemLink  item),
        "   Item date:   " ++ (maybe "" id $ time)]
    
    case time >>= parseDate of
        Just y -> do
            when (threshold < y) $ do
                whenLoud . putStrLn $ "==> New entry added to maildir."
                Maildir.add directory . itemToMail timeZone $ item 
            return y
        _      -> do
            Maildir.add directory . itemToMail timeZone $ item 
            return threshold

downloadRaw :: URI -> IO (Either String String)
downloadRaw uri = do
    result <- simpleHTTP . getRequest $ show uri
    return . either (Left . show) (Right . decodeIfNeeded . rspBody) $ result

-- | Same as Text.Feed.Import.ParseFeedString, but with Either monad.
parseFeedString :: String -> Either String Feed
parseFeedString = maybe
    (Left "Unable to parse XML from raw page.") 
    Right 
    . F.parseFeedString

