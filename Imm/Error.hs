module Imm.Error where

-- {{{ Imports
import qualified Control.Exception as E
import Imm.Util

import Control.Monad.Error

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Text.Encoding as T
import Data.Text.Encoding.Error
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding as TL
import Data.Time as T

import Network.HTTP.Conduit hiding(HandshakeFailed)
import Network.HTTP.Types.Status
import Network.TLS hiding(DecodeError)
import Network.URI as N

import System.IO.Error

import Text.Feed.Query
import Text.Feed.Types

import System.Locale
import System.Log.Logger
import System.Timeout as S
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
        "/!\\ HTTP error: " ++ show (statusCode status) ++ " " ++ (T.unpack . T.decodeUtf8) (statusMessage status)
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


withError :: (Error e, Show e, MonadBase IO m) => String -> ErrorT e m () -> m ()
withError category = runErrorT >=> either (io . errorM category . show) return

localError :: (MonadBase IO m, MonadError ImmError m) => String -> m () -> m ()
localError category f = f `catchError` (io . errorM category . show)

-- | Monad-agnostic version of 'Control.Exception.try'
try :: (MonadBase IO m, MonadError ImmError m) => IO a -> m a
try = (io . E.try) >=> either (throwError . IOE) return

-- | Monad-agnostic version of 'System.timeout'
timeout :: (MonadBase IO m, MonadError ImmError m) => Int -> IO a -> m a
timeout n f = maybe (throwError TimeOut) (io . return) =<< (io $ S.timeout n (io f))


-- {{{ Monad-agnostic version of various error-prone functions
-- | Monad-agnostic version of Data.Text.Encoding.decodeUtf8
decodeUtf8 :: MonadError ImmError m => BL.ByteString -> m TL.Text
decodeUtf8 = either (throwError . UnicodeError) return . TL.decodeUtf8'

-- | Monad-agnostic version of 'Network.URI.parseURI'
parseURI :: (MonadError ImmError m) => String -> m URI
parseURI uri = maybe (throwError $ ParseUriError uri) return $ N.parseURI uri

-- | Monad-agnostic version of 'Data.Time.Format.parseTime'
parseTime :: (MonadError ImmError m) => String -> m UTCTime
parseTime string = maybe (throwError $ ParseTimeError string) return $ T.parseTime defaultTimeLocale "%c" string
-- }}}
