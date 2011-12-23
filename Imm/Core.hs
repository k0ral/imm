module Imm.Core where

-- {{{ Imports
import Imm.Mail
import Imm.Types
import Imm.Util

import qualified Config.Dyre as D
import Config.Dyre.Paths

import Data.Foldable
import Data.Maybe
import qualified Data.String.UTF8 as U
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format

import Network.BSD
import Network.HTTP hiding(Response)
import Network.URI

import System.Console.CmdArgs
import System.Directory
import System.IO
import qualified System.IO.UTF8 as U
import System.IO.Error
import System.Locale
import System.Random

import Text.Feed.Import
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


-- | Default configuration.
defaultParameters :: Parameters
defaultParameters = Parameters {
    mCacheDirectory = Nothing,
    mFeedURIs       = [],
    mMailDirectory  = "rss",
    mError          = Nothing
}
-- }}}


imm :: Parameters -> IO ()
imm = D.wrapMain dyreParameters

-- Entry point
realMain :: Parameters -> IO ()
realMain parameters@Parameters{ mMailDirectory = directory } = do
-- Print configuration error, if any
    maybe (return ()) putStrLn $ mError parameters
    
-- Parse commandline arguments
    options <- getOptions

-- Print in-use paths
    (a, b, c, d, e) <- getPaths dyreParameters 
    whenLoud $ putStrLn (unlines [
        "Current binary:  " ++ a,
        "Custom binary:   " ++ b,
        "Config file:     " ++ c,
        "Cache directory: " ++ d,
        "Lib directory:   " ++ e,
        ""
        ])
        
-- Initialize mailbox
    result <- initMailDir directory
    case result of
        False -> putStrLn $ "Unable to initialize maildir at: " ++ directory
        _     -> realMain' parameters
   
-- 
realMain' :: Parameters -> IO ()
realMain' parameters = do    
-- Retrieve feeds
    let uris = mapMaybe   parseURI $ mFeedURIs parameters
    rawData <- mapIOMaybe downloadRaw uris
    feeds   <- mapIOMaybe rawToFeed rawData
    
-- 
    _ <- mapM (processFeed parameters) feeds 
    
    return ()


initMailDir :: FilePath -> IO Bool
initMailDir directory = do
    root <- try $ createDirectoryIfMissing True directory
    cur  <- try $ createDirectoryIfMissing True $ directory ++ "/cur"
    new  <- try $ createDirectoryIfMissing True $ directory ++ "/new"
    tmp  <- try $ createDirectoryIfMissing True $ directory ++ "/tmp"
    
    case (root, cur, new, tmp) of
        (Right _, Right _, Right _, Right _) -> return True
        _                                    -> return False


processFeed :: Parameters -> ImmFeed -> IO ()
processFeed parameters _f@ImmFeed {mURI = uri, mFeed = feed} = do
    --putStrLn $ "Processing feed: " ++ uri
    putStrLn $ "Feed title:  " ++ (getFeedTitle  feed)
    putStrLn $ "Feed author: " ++ (maybe "No author" id $ getFeedAuthor feed)
    putStrLn $ "Feed home:   " ++ (maybe "No home"   id $ getFeedHome feed)
    
    (_, _, _, d, _) <- getPaths dyreParameters
    let directory = maybe d id $ mCacheDirectory parameters  
    let fileName  = uriToFilePath uri
    
-- 
    oldTime <- try $ readFile (directory ++ "/" ++ fileName)
    let threshold = case oldTime of
          Left _  -> Nothing
          Right x -> parseTime defaultTimeLocale "%F %T %Z" x
    
    lastTime <- foldlM (\acc item -> processItem parameters threshold item >>= (return . (flip maxMaybe acc))) (posixSecondsToUTCTime 0) (feedItems feed) 
    
-- 
    (file, handle) <- openTempFile directory fileName
    hPutStrLn handle (show lastTime)
    hClose handle
    renameFile file (directory ++ "/" ++ fileName)
    
    return ()
    

maxMaybe :: (Ord a) => Maybe a -> a -> a
maxMaybe (Just x) = max x
maxMaybe Nothing  = id
        
getUniqueName :: IO String    
getUniqueName = do
    time     <- getPOSIXTime >>= (return . show)
    hostname <- getHostName
    rand     <- (getStdRandom $ randomR (1,100000) :: IO Int) >>= (return . show)
    
    return $ time ++ "." ++ rand ++ "." ++ hostname
    

processItem :: Parameters -> Maybe UTCTime -> Item -> IO (Maybe UTCTime)
processItem parameters@Parameters{ mMailDirectory = directory } threshold item = do
    putStrLn $ "   Item author: " ++ (maybe "" id $ getItemAuthor item)
    putStrLn $ "   Item title: " ++ (maybe "" id $ getItemTitle item)
    putStrLn $ "   Item URI:   " ++ (maybe "" id $ getItemLink  item)
    
    fileName    <- getUniqueName
    currentTime <- getCurrentTime :: IO UTCTime
    let time = getItemDate item >>= stringToUTC
    print time
    print threshold
    print ""
    
    case (threshold, time) of
        (Just x, Just y) -> case x < y of
            True -> addItemToMailDir (directory ++ "/new/" ++ fileName) item
            _    -> return ()
        _     -> addItemToMailDir (directory ++ "/new/" ++ fileName) item  
        
    return time
        
        
addItemToMailDir :: FilePath -> Item -> IO ()
addItemToMailDir filePath item = U.writeFile filePath $ show (itemToMail item)
  
itemToMail :: Item -> Mail
itemToMail item = defaultMail {
    mReturnPath = "<noreply@anonymous.net>",
    mDate       = stringToUTC $ (maybe "" id $ getItemDate item),
    mFrom       = maybe "Anonymous" id $ getItemAuthor item,
    mSubject    = maybe "Untitled" id $ getItemTitle item,
    mCharset    = "utf-8",
    mContentDisposition = "inline",
    mContent = maybe "Empty" id $ getItemDescription item
}

    
stringToUTC :: String -> Maybe UTCTime
stringToUTC = parseTime defaultTimeLocale "%a, %e %b %Y %T %z"


downloadRaw :: URI -> IO (Maybe (URI, String))
downloadRaw uri = do
    result <- simpleHTTP $ (getRequest . show) uri
    case result of
      Left error_    -> print error_ >> return Nothing
      Right rawPage  -> return $ Just (uri, rspBody rawPage)

rawToFeed :: (URI, String) -> IO (Maybe ImmFeed)
rawToFeed (uri, rawPage) = do
  case parseFeedString rawPage of
    Just x  -> return $ Just ImmFeed{ mURI = uri, mFeed = x}
    Nothing -> putStrLn "Unable to parse XML from raw page." >> return Nothing
