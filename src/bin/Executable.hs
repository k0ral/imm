{-# LANGUAGE NoImplicitPrelude #-}
--module Executable where

-- {{{ Imports
import           Imm
import           Imm.Database.JsonFile
import           Imm.Hooks.Dummy
import           Imm.HTTP.Simple
import           Imm.Logger.Simple
import           Imm.Prelude
import           Imm.XML.Conduit

import           Control.Concurrent.MVar
-- }}}


main :: IO ()
main = do
  logger <- defaultLogger
  manager <- defaultManager
  database <- defaultDatabase :: IO (MVar (JsonFileDatabase FeedTable))

  imm $ mkModulesM manager database logger DummyHooks defaultXmlParser
