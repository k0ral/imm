{-# LANGUAGE FlexibleContexts #-}
module Imm.Feed where

-- {{{ Imports
import Imm.Types
import Imm.Util

import Control.Conditional
import Control.Monad.Error
import Control.Monad.Reader

--import Data.Functor
import qualified Data.Text.Lazy as T
import Data.Time hiding(parseTime)
import Data.Time.Clock.POSIX

import Network.URI as N

import System.Directory
import System.FilePath
import System.IO
import System.Locale

import qualified Text.Atom.Feed as Atom
import Text.Feed.Import as F
import Text.Feed.Query as F
import Text.Feed.Types as F
import Text.XML.Light.Proc
-- }}}

-- {{{ Util
getStateFile :: URI -> FilePath
getStateFile feedUri@URI{ uriAuthority = Just auth } = toFileName =<< ((++ (uriQuery feedUri)) . (++ (uriPath feedUri)) . uriRegName $ auth)
getStateFile feedUri = show feedUri >>= toFileName

toFileName :: Char -> String
toFileName '/' = "."
toFileName '?' = "."
toFileName x = [x]
-- }}}

-- | Monad-agnostic version of 'Text.Feed.Import.parseFeedString'
parseFeedString :: MonadError ImmError m => String -> m Feed
parseFeedString x = maybe (throwError $ ParseFeedError x) return $ F.parseFeedString x

-- | 
printStatus :: (MonadReader Settings m, MonadIO m) => String -> m ()
printStatus feedUri = do
    prefix <- case N.parseURI feedUri of
        Just uri -> do
          lastCheck <- getLastCheck uri
          return $ (lastCheck == posixSecondsToUTCTime 0) ? "[NEW] " ?? ("[Last update: "++ show lastCheck ++ "]")
        _ -> return "[Not an URI]"
    io . putStrLn $ prefix ++ " " ++ feedUri


getLastCheck :: (MonadReader Settings m, MonadIO m) => URI -> m UTCTime
getLastCheck feedUri = do
    directory <- asks mStateDirectory >>= resolve
    result    <- runErrorT $ do
        content <- try $ readFile (directory </> fileName)
        parseTime content
        
    either (const $ return timeZero) return result
  where
    fileName = getStateFile feedUri
    timeZero = posixSecondsToUTCTime 0 


storeLastCheck :: (MonadReader Settings m, MonadIO m, MonadError ImmError m) => URI -> UTCTime -> m ()
storeLastCheck feedUri date = do
    directory <- asks mStateDirectory >>= resolve
    
    (file, stream) <- try $ openTempFile directory fileName
    io $ hPutStrLn stream (formatTime defaultTimeLocale "%c" date)
    io $ hClose stream
    try $ renameFile file (directory </> fileName)
  where
    fileName = getStateFile feedUri
    
-- {{{ Item utilities
getItemLinkNM :: Item -> String 
getItemLinkNM item = maybe "No link found" paragraphy  $ getItemLink item


getItemContent :: Item -> T.Text
getItemContent (AtomItem e) = T.pack . maybe "No content" extractHtml . Atom.entryContent $ e
getItemContent item = T.pack . maybe "Empty" id . getItemDescription $ item

getItemDate :: MonadError ImmError m => Item -> m UTCTime
getItemDate x = maybe (throwError $ ParseItemDateError x) return $ parseDate =<< F.getItemDate x
-- }}}


extractHtml :: Atom.EntryContent -> String
extractHtml (Atom.HTMLContent c) = c
extractHtml (Atom.XHTMLContent c) = strContent c
extractHtml (Atom.TextContent t) = t
extractHtml (Atom.MixedContent a b) = show a ++ show b
extractHtml (Atom.ExternalContent mediaType uri) = show mediaType ++ show uri


paragraphy :: String -> String
paragraphy s = "<p>"++s++"</p>"

