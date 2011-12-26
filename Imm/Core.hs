module Imm.Core where

-- {{{ Imports
import Imm.Mail
import qualified Imm.Maildir as Maildir
import Imm.Types
import Imm.Util

import Codec.Binary.UTF8.String

import qualified Config.Dyre as D
import Config.Dyre.Paths

import Control.Monad hiding(forM_)

import Data.Foldable
import Data.Maybe
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format

import Network.HTTP hiding(Response)
import Network.URI

import System.Console.CmdArgs
import System.Directory
import System.IO
import System.IO.Error
import System.Locale

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
    whenLoud $ putStrLn (unlines [
        "Current binary:  " ++ a,
        "Custom binary:   " ++ b,
        "Config file:     " ++ c,
        "Cache directory: " ++ d,
        "Lib directory:   " ++ e,
        ""])
        
-- Initialize mailbox
    result <- Maildir.init directory
    case result of
        False -> putStrLn $ "Unable to initialize maildir at: " ++ directory
        _     -> realMain' parameters
   
-- 
realMain' :: Parameters -> IO ()
realMain' parameters@Parameters{ mFeedURIs = feedURIs } = do    
-- Retrieve feeds
    rawData <- mapIOMaybe downloadRaw uris
    feeds   <- mapIOMaybe rawToFeed rawData
    
-- 
    _ <- mapM (processFeed parameters) feeds 
    return ()
    
  where
    uris = mapMaybe parseURI feedURIs


processFeed :: Parameters -> ImmFeed -> IO ()
processFeed parameters _f@ImmFeed {mURI = uri, mFeed = feed} = do
    whenLoud $ putStrLn ("Processing feed: " ++ show uri)
    whenLoud $ putStrLn ("Feed title:  " ++ (getFeedTitle feed))
    whenLoud $ putStrLn ("Feed author: " ++ (maybe "No author" id $ getFeedAuthor feed))
    whenLoud $ putStrLn ("Feed home:   " ++ (maybe "No home"   id $ getFeedHome feed))
    
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
            

processItem :: Parameters -> Maybe UTCTime -> Item -> IO (Maybe UTCTime)
processItem parameters@Parameters{ mMailDirectory = directory } threshold item = do
    whenLoud $ putStrLn ("   Item author: " ++ (maybe "" id $ getItemAuthor item))
    whenLoud $ putStrLn ("   Item title: " ++ (maybe "" id $ getItemTitle item))
    whenLoud $ putStrLn ("   Item URI:   " ++ (maybe "" id $ getItemLink  item))
    
    currentTime <- getCurrentTime :: IO UTCTime
    let time = getItemDate item >>= stringToUTC
    print time
    print threshold
    print ""
    
    case (threshold, time) of
        (Just x, Just y) -> when (x < y) $ Maildir.add directory (itemToMail item)
        _                -> Maildir.add directory (itemToMail item)
        
    return time

downloadRaw :: URI -> IO (Maybe (URI, String))
downloadRaw uri = do
    result <- simpleHTTP (getRequest $ show uri)
    case result of
      Left error_    -> print error_ >> return Nothing
      Right rawPage  -> case isUTF8Encoded body of
          False -> return $ Just (uri, body)
          _     -> return $ Just (uri, decodeString body)
        where
          body = rspBody rawPage

rawToFeed :: (URI, String) -> IO (Maybe ImmFeed)
rawToFeed (uri, rawPage) = case parseFeedString rawPage of
    Just x  -> return $ Just ImmFeed{ mURI = uri, mFeed = x}
    Nothing -> putStrLn "Unable to parse XML from raw page." >> return Nothing
