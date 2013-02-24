module Imm.Util where

-- {{{ Imports
import Imm.Error

import qualified Control.Exception as E
import Control.Monad.Base
import Control.Monad.Error

import qualified Data.ByteString.Lazy as BL
import Data.Functor
import Data.Maybe
import Data.Text.Lazy.Encoding hiding(decodeUtf8)
import qualified Data.Text.Lazy as TL
import Data.Time as T
import Data.Time.RFC2822
import Data.Time.RFC3339

import Network.URI as N

import System.FilePath
import System.Locale
import System.Timeout as S
-- }}}


-- | Like '</>' with first argument in IO to build platform-dependent paths.
(>/>) :: (MonadBase IO m) => IO FilePath -> FilePath -> m FilePath
(>/>) a b = io $ (</> b) <$> a

-- {{{ Monadic utilities
-- | Shortcut to 'liftBase' with 'IO' as base monad
io :: MonadBase IO m => IO a -> m a
io = liftBase

-- | Monad-agnostic version of 'Control.Exception.try'
try :: (MonadBase IO m, MonadError ImmError m) => IO a -> m a
try = (io . E.try) >=> either (throwError . IOE) return

-- | Monad-agnostic version of 'System.timeout'
timeout :: (MonadBase IO m, MonadError ImmError m) => Int -> IO a -> m a
timeout n f = maybe (throwError TimeOut) (io . return) =<< (io $ S.timeout n (io f))
-- }}}

-- {{{ Monad-agnostic version of various error-prone functions
-- | Monad-agnostic version of Data.Text.Encoding.decodeUtf8
decodeUtf8 :: MonadError ImmError m => BL.ByteString -> m TL.Text
decodeUtf8 = either (throwError . UnicodeError) return . decodeUtf8'

-- | Monad-agnostic version of 'Network.URI.parseURI'
parseURI :: (MonadError ImmError m) => String -> m URI
parseURI uri = maybe (throwError $ ParseUriError uri) return $ N.parseURI uri

-- | Monad-agnostic version of 'Data.Time.Format.parseTime'
parseTime :: (MonadError ImmError m) => String -> m UTCTime
parseTime string = maybe (throwError $ ParseTimeError string) return $ T.parseTime defaultTimeLocale "%c" string
-- }}}
