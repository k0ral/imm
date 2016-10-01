{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
module Imm.Dyre
  ( Mode(..)
  , defaultMode
  , wrap
  , recompile
  ) where

-- {{{ Imports
import           Imm.Prelude

import           Config.Dyre
import           Config.Dyre.Compile
import           Config.Dyre.Paths
-- }}}

-- | How dynamic reconfiguration process should behave.
data Mode = Normal | Vanilla | ForceReconfiguration | IgnoreReconfiguration
  deriving(Eq, Show)

-- | Default mode is 'Normal', that is: use custom configuration file and recompile if change detected.
defaultMode :: Mode
defaultMode = Normal


-- | Describe the paths used for dynamic reconfiguration
describePaths :: (IsString t, MonadIO m) => m t
describePaths = io $ do
  (a, b, c, d, e) <- getPaths baseParameters
  return $ fromString $ unlines
    [ "Current binary:  " <> a
    , "Custom binary:   " <> b
    , "Config file:     " <> c
    , "Cache directory: " <> d
    , "Lib directory:   " <> e
    ]

-- | Dynamic reconfiguration settings
parameters :: Mode -> (a -> IO ()) -> Params (Either Text a)
parameters mode main = baseParameters
    { configCheck = mode /= Vanilla
    , realMain = main'
    }
  where
    main' (Left e)  = hPutStrLn stderr e
    main' (Right x) = main x

baseParameters :: Params (Either Text a)
baseParameters = defaultParams
  { projectName             = "imm"
  , showError               = const (Left . fromString)
  , ghcOpts                 = ["-threaded"]
  , statusOut               = hPutStrLn stderr
  , includeCurrentDirectory = False
  }

wrap :: Mode -> (a -> IO ()) -> a -> IO ()
wrap mode result args = wrapMain (parameters mode result) (Right args)


-- | Launch a recompilation of the configuration file
recompile :: (MonadIO m) => m (Maybe Text)
recompile = io $ do
  customCompile baseParameters
  fmap fromString <$> getErrorString baseParameters
