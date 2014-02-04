{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}
module Imm.Config where

-- {{{ Imports
import Imm.Util

import Control.Lens
import Control.Monad.Base
import Control.Monad.Reader hiding(forM_)

import Data.Char
import Data.Default
import Data.Foldable hiding(concat)
import Data.Text.ICU.Convert
import Data.Time

import Text.Feed.Types as F
-- }}}

-- {{{ Types
type Format = (Item, Feed) -> String
data MultiPart = MultiPart {
  _contentHeaders :: [String],
  _content        :: String
}

instance Default MultiPart where
  def = MultiPart {
    _contentHeaders = [],
    _content        = ""
  }

makeLenses ''MultiPart

data Config = Config {
    _maildir        :: FilePath,   -- ^ Where mails will be written
    _dateParsers    :: [String -> Maybe UTCTime],  -- ^ List of date parsing functions, will be tried sequentially until one succeeds
    _formatFrom     :: Format,     -- ^ Called to write the From: header of feed mails
    _formatSubject  :: Format,     -- ^ Called to write the Subject: header of feed mails
    _formatParts    :: (Item, Feed) -> [MultiPart], -- ^ Called to write the body of feed mails
    _multiPartsHeader :: String -> String, -- ^ Called to write the header preceding all email body
    _partsSeparator :: String,     -- ^ Separator used between parts of the email
    _partHeader     :: [String],   -- ^ Header of each part of the email
    _decoder        :: String      -- ^ 'Converter' name used to decode the HTTP response from a feed URI
    -- _decoder        :: BL.ByteString -> Maybe TL.Text   -- ^ Called to decode the HTTP response from a feed URI
}

makeLenses ''Config


-- | 'MonadReader' for 'Config'
class ConfigReader m where
    readConfig :: Simple Lens Config a -> m a

instance (Monad m) => ConfigReader (ReaderT Config m) where
    readConfig l = return . view l =<< ask

instance ConfigReader ((->) Config) where
    readConfig l = view l
-- }}}

-- | Return the decoder corresponding to the converter name set in 'Config'.
getDecoder :: (ConfigReader m, MonadBase IO m) => m Converter
getDecoder = do
    converterName <- readConfig decoder
    io $ open converterName Nothing

-- | Return the Haskell code to write in the configuration file to add a feed.
addFeeds :: MonadBase IO m => [(String, [String])] -> m ()
addFeeds feeds = io . forM_ feeds $ \(groupTitle, uris) -> do
    putStrLn $ "-- Group " ++ groupTitle
    putStrLn $ map toLower (concat . words $ groupTitle) ++ " = ["
    forM_ uris (\uri -> putStrLn $ "    " ++ show uri ++ ",")
    putStrLn "]"
    putStrLn ""
