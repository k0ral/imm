module Imm.Feed where

-- {{{ Imports
import Imm.Types
import Imm.Util

import Control.Error
--import Control.Exception

import qualified Data.Text.Lazy as T
import Data.Time
import Data.Time.Clock.POSIX

import System.Directory
import System.FilePath
import System.IO
import System.Locale

import Text.Atom.Feed
import Text.Feed.Query
import Text.Feed.Types
import Text.XML.Light.Proc
-- }}}


getLastCheck :: Settings -> ImmFeed -> IO UTCTime
getLastCheck settings (uri, _feed) = do
    directory <- resolve $ mStateDirectory settings
    
    result <- runEitherT $ do
        content <- fmapLT OtherError . tryIO $ readFile (directory </> fileName)
        EitherT . return $ parseTime' content

    either 
        (\e -> print e >> return timeZero)
        return
        result
  where
    fileName = show uri >>= escapeFileName
    timeZero = posixSecondsToUTCTime $ 0 
    parseTime' string = note (ParseTimeError string) $ parseTime defaultTimeLocale "%c" string


storeLastCheck :: Settings -> ImmFeed -> UTCTime -> EitherT ImmError IO ()
storeLastCheck settings (uri, _) date = do
    directory <- io . resolve $ mStateDirectory settings
    let fileName = show uri >>= escapeFileName
    
    (file, stream) <- fmapLT OtherError . tryIO $ openTempFile directory fileName
    io $ hPutStrLn stream (formatTime defaultTimeLocale "%c" date)
    io $ hClose stream
    fmapLT OtherError . tryIO $ renameFile file (directory </> fileName)

getItemLinkNM :: Item -> String 
getItemLinkNM item = maybe "No link found" paragraphy  $ getItemLink item

-- ce "magic operator" semble pas défini dans les libs haskell -> WTF ?
(><) :: a -> (a -> b) -> b
(><) a b = b a

paragraphy :: String -> String
paragraphy s = "<p>"++s++"</p>"


getItemContent :: Item -> T.Text
getItemContent (AtomItem e) = T.pack . maybe "No content" extractHtml . entryContent $ e
getItemContent item = T.pack . maybe "Empty" id . getItemDescription $ item


extractHtml :: EntryContent -> String
extractHtml (HTMLContent c) = c
extractHtml (XHTMLContent c) = strContent c
extractHtml (TextContent t) = t
extractHtml (MixedContent a b)= show a ++ show b
extractHtml (ExternalContent mediaType uri) = show mediaType ++ show uri


buildMailBody :: Item -> T.Text
buildMailBody item = 
    T.unlines $ map ((><) item) [T.pack . getItemLinkNM, getItemContent]
