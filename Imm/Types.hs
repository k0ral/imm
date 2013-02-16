{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, ScopedTypeVariables, RankNTypes, FlexibleContexts #-}
module Imm.Types where

-- {{{ Imports
--import Control.Exception
import Control.Monad.Error

import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Data.Text.Encoding
import Data.Text.Encoding.Error
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time

import Network.HTTP.Conduit hiding(HandshakeFailed)
import Network.HTTP.Types.Status
import Network.URI
--import Network.Stream
import Network.TLS hiding(DecodeError)

import Prelude hiding(catch)

import System.Console.CmdArgs
import System.IO.Error hiding(catch)

import Text.Feed.Query
import Text.Feed.Types
-- }}}

-- {{{ Error handling
data ImmError = 
    OtherError         String
  | HTTPError          HttpException
  | TLSError           HandshakeFailed
  | UnicodeError       UnicodeException
  | ParseUriError      String
  | ParseTimeError     String
  | ParseItemDateError Item
  | ParseFeedError     String
  | IOE                IOError
  | TimeOut

instance Show ImmError where
    show (OtherError e)            = e
    show (HTTPError (StatusCodeException status _headers)) = 
        "/!\\ HTTP error: " ++ show (statusCode status) ++ " " ++ (T.unpack . decodeUtf8) (statusMessage status)
    show (HTTPError e)             = "/!\\ HTTP error: " ++ show e
    show (TLSError (HandshakeFailed e)) = "/!\\ TLS error: " ++ show e
    show (UnicodeError (DecodeError e _)) = e
    show (UnicodeError (EncodeError e _)) = e
    show (ParseUriError raw)       = "/!\\ Cannot parse URI: " ++ raw
    show (ParseItemDateError item) = unlines [
        "/!\\ Cannot parse date from item: ",
        "    title: "       ++ (show $ getItemTitle item),
        "    link:"         ++ (show $ getItemLink item),
        "    publish date:" ++ (show $ getItemPublishDate item),
        "    date:"         ++ (show $ getItemDate item)]
    show (ParseTimeError raw)      = "/!\\ Cannot parse time: " ++ raw
    show (ParseFeedError raw)      = "/!\\ Cannot parse feed: " ++ raw
    show (IOE e)                   = "/!\\ IO error: " ++ ioeGetLocation e ++ ": " ++ fromMaybe "" (ioeGetFileName e) ++ " " ++ ioeGetErrorString e
    show TimeOut                   = "/!\\ Process has timed out"

instance Error ImmError where
    strMsg = OtherError
-- }}}

-- {{{ Settings type
-- | Available commandline options (cf imm -h)
data CliOptions = CliOptions {
    mCheck        :: Bool,
    mFeedURI      :: Maybe String,
    mImportOPML   :: Bool,
    mList         :: Bool,
    mMarkAsRead   :: Bool,
    mMarkAsUnread :: Bool,
    mUpdate       :: Bool
} deriving (Data, Typeable, Show, Eq)

data Settings = Settings {
    mStateDirectory :: IO FilePath,                                    -- ^ Where feeds' state (last update time) will be stored
    mMaildir        :: IO FilePath,                                    -- ^ Where mails will be written
    mFromBuilder    :: (Item, Feed) -> String,                         -- ^ Called to write the From: header of feed mails
    mSubjectBuilder :: (Item, Feed) -> TL.Text,                        -- ^ Called to write the Subject: header of feed mails
    mBodyBuilder    :: (Item, Feed) -> TL.Text,                        -- ^ Called to write the body of feed mails (sic!)
    mDecoder        :: (MonadIO m, MonadError ImmError m) => Decoder m -- ^ Called when decoding the HTTP response from a feed URI
}

type Decoder m      = BL.ByteString -> m TL.Text
type CustomSettings = Settings -> Settings
-- }}}

-- {{{ Feed types
type FeedList  = [(CustomSettings, String)]
type ImmFeed   = (URI, Feed)
-- }}}

data Mail = Mail {
    mReturnPath         :: String,
    mDate               :: Maybe ZonedTime,
    mFrom               :: String,
    mSubject            :: TL.Text,
    mMIME               :: String,
    mCharset            :: String,
    mContentDisposition :: String,
    mBody               :: TL.Text
}
