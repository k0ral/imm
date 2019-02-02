{-# LANGUAGE NoImplicitPrelude #-}
--module Executable where

-- {{{ Imports
import           Imm
import           Imm.Database.JsonFile as Database
import           Imm.Hooks.Dummy as Hooks
import           Imm.HTTP.Simple as HTTP
import           Imm.Logger.Simple as Logger
import           Imm.Prelude
import           Imm.XML.Conduit as XML
-- }}}


main :: IO ()
main = do
  logger <- Logger.mkHandle <$> defaultLogger
  database <- Database.mkHandle <$> defaultDatabase
  httpClient <- HTTP.mkHandle <$> defaultManager

  imm logger database httpClient hooks xmlParser

xmlParser :: XML.Handle IO
xmlParser = XML.mkHandle defaultXmlParser

hooks :: Hooks.Handle IO
hooks = Hooks.mkHandle


