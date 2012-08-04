{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Imm.Types where

-- {{{ Imports
--import Control.Exception
import Control.Monad.Error

import Data.Text.Encoding
import Data.Text.Encoding.Error
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time

import Network.HTTP.Conduit hiding(HandshakeFailed)
import Network.HTTP.Types.Status
import Network.URI
--import Network.Stream
import Network.TLS

import Prelude hiding(catch)

import System.Console.CmdArgs
import System.IO.Error hiding(catch)

import Text.Feed.Query
import Text.Feed.Types
-- }}}

-- {{{ Error handling
-- | Errors that can be returned by an Imm process
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
    show (IOE e)                   = "/!\\ IO error: " ++ ioeGetLocation e ++ ": " ++ maybe "" id (ioeGetFileName e) ++ " " ++ ioeGetErrorString e
    show TimeOut                   = "/!\\ Process has timed out"

instance Error ImmError where
    strMsg x = OtherError x
-- }}}

-- {{{ Settings type
data CliOptions = CliOptions {
--    mCheck        :: Bool,
    mFeedURI      :: Maybe String,
    mImportOPML   :: Bool,
    mList         :: Bool,
    mMarkAsRead   :: Bool,
    mMarkAsUnread :: Bool,
    mUpdate       :: Bool,
    mDenyReconf   :: Bool,         -- ^ Do not recompile configuration even if it has changed
    mMasterBinary :: Maybe String
} deriving (Data, Typeable, Show, Eq)

-- | Set of settings for imm
data Settings = Settings {
    mStateDirectory :: PortableFilePath,
    mMaildir        :: PortableFilePath,
    mFromBuilder    :: (Item, Feed) -> String,
    mSubjectBuilder :: (Item, Feed) -> TL.Text,
    mBodyBuilder    :: (Item, Feed) -> TL.Text   -- ^ sic!
}

-- | 
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

-- {{{ Generic file paths
-- | Set of reference directories, typically used to build FilePath-s
data RefDirs = RefDirs {
    mHome          :: FilePath,        -- ^ Home directory
    mTemporary     :: FilePath,        -- ^ Temporary files directory
    mConfiguration :: FilePath,        -- ^ Configuration directory
    mData          :: FilePath         -- ^ Data directory
}

type PortableFilePath = RefDirs -> FilePath
-- }}}
