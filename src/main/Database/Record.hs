{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.Record
  ( FeedStatus(..)
  , markFeedAsUnprocessed
  , touchFeed
  , FeedItemStatus(..)
  , markItemAsProcessed
  , markItemAsUnprocessed
  , FeedRecord(..)
  , mkFeedRecord
  , FeedItemRecord(..)
  , prettyItemRecord
  , mkFeedItemRecord
  , Inserted
  , NotInserted
  ) where

import           Control.Monad.Time
import           Data.Aeson
import           Data.Time
import           Imm.Feed
import           Imm.Pretty


-- | Stateful information on a feed
data FeedStatus = FeedStatus
  { _feedTags       :: Set Text
  , _feedLastUpdate :: Maybe UTCTime
  } deriving(Eq, Generic, Ord, Read, Show, Typeable)

feedStatusOptions :: Options
feedStatusOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop (length @[] "_feed")
  , omitNothingFields = True
  }

instance ToJSON FeedStatus where
  toJSON     = genericToJSON feedStatusOptions
  toEncoding = genericToEncoding feedStatusOptions

instance FromJSON FeedStatus where
  parseJSON = genericParseJSON feedStatusOptions

instance Pretty FeedStatus where
  pretty FeedStatus{..} = tags <++> "Last update:" <+> lastUpdate
    where tags = sep $ map ((<>) "#" . pretty) $ toList _feedTags
          lastUpdate = maybe "never" prettyTime _feedLastUpdate

markFeedAsUnprocessed :: FeedStatus -> FeedStatus
markFeedAsUnprocessed status = status
  { _feedLastUpdate = Nothing
  }

touchFeed :: FeedStatus -> IO FeedStatus
touchFeed status = do
  utcTime <- currentTime
  return $ status { _feedLastUpdate = Just utcTime }


-- | Stateful information on a feed item
newtype FeedItemStatus = FeedItemStatus
  { _isProcessed :: Bool
  } deriving(Eq, Generic, Ord, Read, Show, Typeable)

instance Pretty FeedItemStatus where
  pretty FeedItemStatus{..} = pretty _isProcessed

feedItemStatusOptions :: Options
feedItemStatusOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_'
  , omitNothingFields = True
  }

instance ToJSON FeedItemStatus where
  toJSON     = genericToJSON feedItemStatusOptions
  toEncoding = genericToEncoding feedItemStatusOptions

instance FromJSON FeedItemStatus where
  parseJSON = genericParseJSON feedItemStatusOptions

markItemAsProcessed :: FeedItemStatus -> FeedItemStatus
markItemAsProcessed status = status { _isProcessed = True }

markItemAsUnprocessed :: FeedItemStatus -> FeedItemStatus
markItemAsUnprocessed status = status { _isProcessed = False }


-- | Consistent record of information for a single feed
data FeedRecord status = FeedRecord
  { _feedKey        :: StatusUID status
  , _feedLocation   :: FeedLocation
  , _feedDefinition :: FeedDefinition
  , _feedStatus     :: FeedStatus
  }

deriving instance (Eq (StatusUID s)) => Eq (FeedRecord s)
deriving instance (Ord (StatusUID s)) => Ord (FeedRecord s)
deriving instance (Show (StatusUID s)) => Show (FeedRecord s)

instance Pretty (PrettyKey (FeedRecord Inserted)) where
  pretty (PrettyKey record) = pretty $ _feedKey record

instance Pretty (PrettyKey (FeedRecord NotInserted)) where
  pretty (PrettyKey record) = pretty $ _feedLocation record

mkFeedRecord :: FeedLocation -> FeedDefinition -> FeedStatus -> FeedRecord NotInserted
mkFeedRecord = FeedRecord ()


-- | Consistent record of information for a single feed item
data FeedItemRecord status = FeedItemRecord
  { _itemKey        :: StatusUID status
  , _itemFeedKey    :: UID
  , _itemDefinition :: FeedItem
  , _itemStatus     :: FeedItemStatus
  }

deriving instance (Eq (StatusUID s)) => Eq (FeedItemRecord s)
deriving instance (Ord (StatusUID s)) => Ord (FeedItemRecord s)
deriving instance (Show (StatusUID s)) => Show (FeedItemRecord s)

instance Pretty (PrettyKey (FeedItemRecord Inserted)) where
  pretty (PrettyKey record) = pretty $ _itemKey record

instance Pretty (PrettyName (FeedItemRecord a)) where
  pretty (PrettyName record) = pretty $ _itemTitle $ _itemDefinition record

prettyItemRecord :: Pretty (StatusUID s) => FeedItemRecord s -> Doc AnsiStyle
prettyItemRecord FeedItemRecord{..} = pretty _itemKey <+> magenta (pretty _itemFeedKey)
  <++> pretty _itemDefinition
  <++> pretty _itemStatus

mkFeedItemRecord :: UID -> FeedItem -> FeedItemStatus -> FeedItemRecord NotInserted
mkFeedItemRecord = FeedItemRecord ()


data Inserted
data NotInserted

type family StatusUID s where
  StatusUID Inserted = UID
  StatusUID NotInserted = ()
