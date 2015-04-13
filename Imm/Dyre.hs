module Imm.Dyre (
    wrap,
    recompile,
) where

-- {{{ Imports
import Imm.Util

import Config.Dyre
import Config.Dyre.Compile
import Config.Dyre.Paths

import Control.Monad
import Control.Monad.Trans.Control

import System.IO
import System.Log.Logger
-- }}}


nullMain :: a -> IO ()
nullMain = const $ return ()

-- Print various paths used for dynamic reconfiguration
showPaths :: MonadBase IO m => m String
showPaths = io $ do
    (a, b, c, d, e) <- getPaths $ parameters nullMain
    return . unlines $ [
        "Current binary:  " ++ a,
        "Custom binary:   " ++ b,
        "Config file:     " ++ c,
        "Cache directory: " ++ d,
        "Lib directory:   " ++ e, []]

-- | Dynamic reconfiguration settings
parameters :: (a -> IO ()) -> Params (Either String a)
parameters main = defaultParams {
    projectName             = "imm",
    showError               = const Left,
    realMain                = main',
    ghcOpts                 = ["-threaded"],
    statusOut               = hPutStrLn stderr,
    includeCurrentDirectory = False}
  where
    main' (Left e)  = putStrLn e
    main' (Right x) = do
        debugM "imm.dyre" =<< showPaths
        main x

wrap :: (MonadBaseControl IO m) => Bool -> (a -> m ()) -> a -> m ()
wrap vanilla main args = liftBaseWith $ \runInIO -> wrapMain ((parameters (void . runInIO . main)) { configCheck = not vanilla }) $ Right args

-- | Launch a recompilation of the configuration file
recompile :: IO (Maybe String)
recompile = do
    customCompile  $ parameters nullMain
    getErrorString $ parameters nullMain
