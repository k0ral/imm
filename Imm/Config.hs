{-# LANGUAGE OverlappingInstances, TemplateHaskell #-}
module Imm.Config (
-- * Types
    FromFormat(FromFormat),
    SubjectFormat(SubjectFormat),
    BodyFormat(BodyFormat),
    Config,
    maildir,
    fileDatabase,
    dateParsers,
    formatFrom,
    formatSubject,
    formatBody,
    decoder,
    ConfigReader(..),
    withConfig,
-- * Misc
    addFeeds,
) where

-- {{{ Imports
import Imm.Database
import Imm.Error
import Imm.Feed (FeedParser)
import qualified Imm.Feed as F
import Imm.Maildir (Maildir, MaildirWriter(..))
import Imm.HTTP (Decoder(..))
import qualified Imm.Mail as Mail
import Imm.Util

import Control.Lens hiding((??))
import Control.Monad.Error hiding(forM_, guard)
import Control.Monad.Reader hiding(forM_, guard)

import Data.Foldable hiding(concat)
import Data.Text.ICU.Convert
import qualified Data.Text.Lazy as TL
import Data.Time as T
import Data.Time.RFC2822
import Data.Time.RFC3339

import Prelude hiding(init)

import Text.Feed.Query as F
-- import Text.Feed.Types as F

import System.Directory
-- import System.Environment.XDG.BaseDir
import System.Locale
-- }}}

-- {{{ Types
newtype FromFormat    = FromFormat    { unFromFormat    :: Mail.Format }
newtype SubjectFormat = SubjectFormat { unSubjectFormat :: Mail.Format }
newtype BodyFormat    = BodyFormat    { unBodyFormat    :: Mail.Format }

instance Default FromFormat where
    def = FromFormat $ \(item, feed) -> fromMaybe (getFeedTitle feed) $ getItemAuthor item

instance Default SubjectFormat where
    def = SubjectFormat $ \(item, _feed) -> fromMaybe "Untitled" $ getItemTitle item

instance Default BodyFormat where
    def = BodyFormat $ \(item, _feed) -> let
                                           link        = fromMaybe "No link found." $ getItemLink item
                                           content     = F.getItemContent item
                                           description = fromMaybe "No description." $ getItemDescription item
                                         in "<p>" ++ link ++ "</p><p>" ++ (null content ? description ?? content) ++ "</p>"


-- | The only exported constructor is through 'Default' class.
data Config = Config {
    _maildir        :: Maildir,                    -- ^ Where mails will be written
    _fileDatabase   :: FileDatabase,               -- ^ Database configuration, used to store resilient information (basically: last update time)
    _dateParsers    :: [String -> Maybe UTCTime],  -- ^ List of date parsing functions, will be tried sequentially until one succeeds
    _formatFrom     :: FromFormat,                 -- ^ Called to write the From: header of feed mails
    _formatSubject  :: SubjectFormat,              -- ^ Called to write the Subject: header of feed mails
    _formatBody     :: BodyFormat,                 -- ^ Called to write the body of feed mails (sic!)
    _decoder        :: String                      -- ^ 'Converter' name used to decode the HTTP response from a feed URI
}

makeLenses ''Config

instance Default (IO Config) where
    def = do
        theDatabase <- def
        mailDir     <- getHomeDirectory >/> "feeds"
        return Config {
            _maildir       = mailDir,
            _fileDatabase  = theDatabase,
            _dateParsers   = [
                return . zonedTimeToUTC <=< readRFC2822,
                return . zonedTimeToUTC <=< readRFC3339,
                T.parseTime defaultTimeLocale "%a, %d %b %G %T",
                T.parseTime defaultTimeLocale "%Y-%m-%d",
                T.parseTime defaultTimeLocale "%e %b %Y",
                T.parseTime defaultTimeLocale "%a, %e %b %Y %k:%M:%S %z",
                T.parseTime defaultTimeLocale "%a, %e %b %Y %T %Z"],
            _formatFrom    = def,
            _formatSubject = def,
            _formatBody    = def,
            _decoder       = "UTF-8"
        }

instance (Monad m) => FeedParser (ReaderT Config m) where
    parseDate date = return . listToMaybe . {-map T.zonedTimeToUTC .-} catMaybes =<< tryParsers strippedDate
      where
        tryParsers string = return . map ($ string) =<< asks (view dateParsers)
        strippedDate      = TL.unpack . TL.strip . TL.pack $ date

instance (Applicative m, MonadBase IO m) => Decoder (ReaderT Config m) where
    converter = io . (`open` Nothing) =<< asks (view decoder)

instance (MonadBase IO m) => DatabaseReader (ReaderT Config m) where
    getLastCheck = withReaderT (view fileDatabase) . getLastCheck

instance (MonadError ImmError m, MonadBase IO m) => DatabaseWriter (ReaderT Config m) where
    storeLastCheck uri = withReaderT (view fileDatabase) . storeLastCheck uri
    forget             = withReaderT (view fileDatabase) . forget

instance (MonadBase IO m, MonadError ImmError m) => MaildirWriter (ReaderT Config m) where
    init       = do
        theMaildir <- asks $ view maildir
        lift $ runReaderT init theMaildir
    write mail = do
        theMaildir <- asks $ view maildir
        lift $ runReaderT (write mail) theMaildir


instance (Monad m) => Mail.MailFormatter (ReaderT Config m) where
    formatFrom    = asks $ unFromFormat    . view formatFrom
    formatSubject = asks $ unSubjectFormat . view formatSubject
    formatBody    = asks $ unBodyFormat    . view formatBody


-- | 'MonadReader' for 'Config'
class ConfigReader m where
    readConfig  :: Simple Lens Config a -> m a
    localConfig :: (Config -> Config) -> m a -> m a

instance (Monad m) => ConfigReader (ReaderT Config m) where
    readConfig l = return . view l =<< ask
    localConfig  = local


withConfig :: (MonadBase IO m) => (Config -> Config) -> ReaderT Config m a -> m a
withConfig f g = do
    theConfig <- f <$> io def
    runReaderT g theConfig
-- }}}


-- | Return the Haskell code to write in the configuration file to add feeds.
addFeeds :: (MonadBase IO m) => [(String, [String])] -> m ()
addFeeds feeds = do
    io . putStrLn . unlines $
        "import Imm":
        "import Control.Lens":
        "import System.FilePath":
        "":
        "main :: IO ()":
        "main = imm myFeeds":
        "":
        "maildirRoot = \"/home/<user>/feeds\"   -- TODO: fill <user>":
        "":
        ("myFeeds = concat [" ++ intercalate ", " (map (map toLower . concat . words . fst) feeds) ++ "]"):
        []

    forM_ feeds addFeedsGroup

addFeedsGroup :: (MonadBase IO m) => (String, [String]) -> m ()
addFeedsGroup (groupTitle, uris) = io $ do
    -- guard (not $ null uris)
    putStr . unlines $
        ("-- Group " ++ groupTitle):
        (groupID ++ "Config = set maildir (maildirRoot </> \"" ++ groupID ++ "\")"):
        (groupID ++ "       = zip (repeat " ++ groupID ++ "Config) $"):
        []
    putStr . unlines $ map (\u -> "    " ++ show u ++ ":") uris
    putStrLn "    []"
    putStrLn ""
  where
    groupID = map toLower . concat . words $ groupTitle
