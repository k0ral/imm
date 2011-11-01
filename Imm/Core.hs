module Imm.Core where

import Imm.Types
import Imm.Util

import qualified Config.Dyre as D
import Config.Dyre.Paths

import Network.HTTP

import System.IO

import Text.Feed.Constructor
import Text.Feed.Import
import Text.Feed.Query
import Text.XML.Light.Input
import Text.XML.Light.Types


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
    mFeedURIs = ["http://kernel.org/kdist/rss.xml", "http://store.steampowered.com/feeds/news.xml"],
    mError = Nothing
}
-- }}}


imm :: Parameters -> IO ()
imm = D.wrapMain dyreParameters

realMain :: Parameters -> IO ()
realMain parameters = do
    rawPages <- mapIOMaybe downloadRawPage $ mFeedURIs parameters
    feeds    <- mapIOMaybe rawToFeed rawPages
    
    _ <- flip mapM feeds $ \f@ImmFeed {mFeed = feed} -> do
      putStrLn $ "Feed title:  " ++ (getFeedTitle  feed)
      putStrLn $ "Feed author: " ++ (maybe "No author" id $ getFeedAuthor feed)
      putStrLn $ "Feed home:   " ++ (maybe "No home" id $ getFeedHome feed)
      
      flip mapM (feedItems feed) $ \item -> do
        putStrLn $ "   Item title: " ++ (maybe "" id $ getItemTitle item)
        putStrLn $ "   Item URI:   " ++ (maybe "" id $ getItemLink  item)
        
      
      
    --print xmlPages
    
    --map printTitle feeds
    
    return ()

--uriToFeed :: String -> IO ImmFeed
--uriToFeed uri = do
--  page <- download uri
--  return $ ImmFeed{ mURI = uri, mFeed = maybe Nothing (Just . feedFromXML) . parseXMLDoc $ page}

downloadRawPage :: String -> IO (Maybe (String, String))
downloadRawPage uri = do
    result <- simpleHTTP (getRequest uri)
    case result of
      Left error     -> print error >> return Nothing
      Right rawPage  -> return $ Just (uri, rspBody rawPage)

rawToFeed :: (String, String) -> IO (Maybe ImmFeed)
rawToFeed (uri, rawPage) = do
  case parseFeedString rawPage of
    Just x  -> return $ Just ImmFeed{ mURI = uri, mFeed = x}
    Nothing -> putStrLn "Unable to parse XML from raw page." >> return Nothing
