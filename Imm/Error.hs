module Imm.Error where

-- {{{ Imports
import Control.Monad.Error

import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Text.Encoding.Error

import Network.HTTP.Conduit hiding(HandshakeFailed)
import Network.HTTP.Types.Status
import Network.TLS hiding(DecodeError)

import System.IO.Error

import Text.Feed.Query
import Text.Feed.Types
-- }}}

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
    show (HTTPError (StatusCodeException status _headers _cookieJar)) =
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
    show (IOE e)                   = "/!\\ IO error [" ++ ioeGetLocation e ++ "]: " ++ fromMaybe "" (ioeGetFileName e) ++ " " ++ ioeGetErrorString e
    show TimeOut                   = "/!\\ Process has timed out"

instance Error ImmError where
    strMsg = OtherError
