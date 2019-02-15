{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
module Imm.Dyre
  ( Mode(..)
  , defaultMode
  , describePaths
  , wrap
  , recompile
  ) where

-- {{{ Imports
import           Imm.Pretty

import           Config.Dyre
import           Config.Dyre.Compile
import           Config.Dyre.Paths

import           System.IO
-- }}}

-- | How dynamic reconfiguration process should behave.
data Mode = Normal | Vanilla | ForceReconfiguration | IgnoreReconfiguration
  deriving(Eq, Show)

-- | Default mode is 'Normal', that is: use custom configuration file and recompile if change detected.
defaultMode :: Mode
defaultMode = Normal


-- | Describe the paths used for dynamic reconfiguration
describePaths :: (MonadIO m) => m (Doc AnsiStyle)
describePaths = io $ do
  (a, b, c, d, e) <- getPaths baseParameters
  return $ vsep
    [ "Current binary" <+> equals <+> magenta (fromString a)
    , "Custom binary" <+> equals <+> magenta (fromString b)
    , "Config file" <+> equals <+> magenta (fromString c)
    , "Cache directory" <+> equals <+> magenta (fromString d)
    , "Lib directory" <+> equals <+> magenta (fromString e)
    ]

-- | Dynamic reconfiguration settings
parameters :: Mode -> (a -> IO ()) -> Params (Either String a)
parameters mode main = baseParameters
    { configCheck = mode /= Vanilla
    , realMain = main'
    }
  where
    main' (Left e)  = hPutStrLn stderr e
    main' (Right x) = main x

baseParameters :: Params (Either String a)
baseParameters = defaultParams
  { projectName             = "imm"
  , showError               = const Left
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
