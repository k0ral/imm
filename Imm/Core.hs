module Imm.Core where

-- {{{ Imports
import Imm.Types
import Imm.Util

import qualified Config.Dyre as D
import Config.Dyre.Paths

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy  as BL
import Data.Maybe
import qualified Data.Text             as T
import qualified Data.Text.Lazy        as TL

import Network.HaskellNet.SMTP hiding(sendMail)
import Network.HTTP hiding(Response)
import Network.Mail.Mime
import Network.URI

import System.Console.CmdArgs
import System.IO

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
    mMailTo         = Nothing,
    mSMTP           = Nothing,
    mFeedURIs       = [],
    mError          = Nothing
}
-- }}}


imm :: Parameters -> IO ()
imm = D.wrapMain dyreParameters

-- Entry point
realMain :: Parameters -> IO ()
realMain parameters = do
-- Print configuration error, if any
    maybe (return ()) putStrLn $ mError parameters
    
-- Parse commandline arguments
    options <- getOptions

-- Print in-use paths
    (a, b, c, d, e) <- getPaths dyreParameters 
    whenLoud $ do
        putStrLn ("Current binary:  " ++ a)
        putStrLn ("Custom binary:   " ++ b)
        putStrLn ("Config file:     " ++ c)
        putStrLn ("Cache directory: " ++ d)
        putStrLn ("Lib directory:   " ++ e)
        putStrLn ""
    
    let uris = mapMaybe   parseURI $ mFeedURIs parameters
    rawData <- mapIOMaybe downloadRaw uris
    feeds   <- mapIOMaybe rawToFeed rawData
    
    
    _ <- mapM (processFeed parameters) feeds 
    
    return ()


processFeed :: Parameters -> ImmFeed -> IO ()
processFeed parameters _f@ImmFeed {mURI = uri, mFeed = feed} = do
    putStrLn $ "Feed title:  " ++ (getFeedTitle  feed)
    putStrLn $ "Feed author: " ++ (maybe "No author" id $ getFeedAuthor feed)
    putStrLn $ "Feed home:   " ++ (maybe "No home"   id $ getFeedHome feed)
    
    (_, _, _, d, _) <- getPaths dyreParameters
    let directory = maybe d id $ mCacheDirectory parameters  
    let fileName  = uriToFilePath uri
    withFile (directory ++ "/" ++ fileName) ReadWriteMode $ \handle -> do      
        _ <- mapM (processItem parameters) (feedItems feed) 
        return ()
        
processItem :: Parameters -> Item -> IO ()
processItem parameters item = do
    putStrLn $ "   Item title: " ++ (maybe "" id $ getItemTitle item)
    putStrLn $ "   Item URI:   " ++ (maybe "" id $ getItemLink  item)
    
    maybe (return ()) (\(to:smtp:_) -> sendMail to smtp item) $ sequence [mMailTo parameters, mSMTP parameters]
    
    return ()
    

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


sendMail :: String -> String -> Item -> IO ()
sendMail to smtp item = do
  mail <- simpleMail (T.pack to) author subject TL.empty content [] >>= renderMail'
  
  doSMTPPort smtp 25 $ \connection -> do
    _ <- sendCommand connection (HELO "noreply.net")
    _ <- sendCommand connection (MAIL "<imm@noreply.net>")
    _ <- sendCommand connection (RCPT to)
    _ <- sendCommand connection (DATA $ B.concat (BL.toChunks mail))
    _ <- sendCommand connection (QUIT)
    return ()
  where  
    author  = T.pack  $ maybe "Anonymous" id (getItemAuthor item)
    subject = T.pack  $ maybe "Untitled" id (getItemTitle item)
    content = TL.pack $ maybe link (\d -> link ++ "<hr/>" ++ d) (getItemDescription item)
    link    = maybe "" id $ getItemLink item

