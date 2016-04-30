{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
--module Executable where

-- {{{ Imports
import           Imm
import           Imm.Database.JsonFile
import           Imm.Feed
import           Imm.Hooks
import           Imm.HTTP.Simple
import           Imm.Logger.Simple
import           Imm.Prelude

import           System.Exit
-- }}}

mkDummyCoHooks :: (MonadIO m, MonadThrow m) => () -> CoHooksF m ()
mkDummyCoHooks _ = CoHooksF coOnNewElement where
  coOnNewElement _ _ = do
    io $ putStrLn "No hook defined."
    throwM $ ExitFailure 1


main :: IO ()
main = do
  logger <- defaultLogger
  manager <- defaultManager
  database <- defaultDatabase

  imm (mkCoHttpClient, manager) (mkCoDatabase, database) (mkCoLogger, logger) (mkDummyCoHooks, ())
