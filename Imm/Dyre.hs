module Imm.Dyre where

-- {{{ Imports
import Imm.Options
import Imm.Util

import Config.Dyre
import Config.Dyre.Paths

import Control.Lens
import Control.Monad
import Control.Monad.Base

import System.IO
-- }}}


-- | Print various paths used for dynamic reconfiguration
printPaths :: MonadBase IO m => m ()
printPaths = io $ do
    (a, b, c, d, e) <- getPaths (parameters $ const $ return ())
    putStrLn . unlines $ [
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
    main' (Right x) = main x

wrap :: (a -> IO ()) -> CliOptions -> a -> IO ()
wrap main opts args = do
    when (opts^.verbose) printPaths
    wrapMain ((parameters main) { configCheck = not $ opts^.vanilla }) $ Right args
