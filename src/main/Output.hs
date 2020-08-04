{-# LANGUAGE OverloadedStrings #-}
module Output where

import           Control.Concurrent.Async
import           Control.Concurrent.STM.TMChan
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Imm.Pretty

-- * Types

-- | Handle to push messages to the program's output
newtype Handle m = Handle
  { putDocLn :: Doc AnsiStyle -> m ()
  }

withHandle :: (Handle IO -> IO ()) -> IO ()
withHandle f = do
  channel <- newTMChanIO

  thread <- async $ fix $ \recurse -> do
    maybeMessage <- atomically $ readTMChan channel
    forM_ maybeMessage $ \message -> do
      putLTextLn $ renderLazy $ layoutPretty defaultLayoutOptions message
      recurse

  f $ Handle { putDocLn = atomically . writeTMChan channel }
  atomically (closeTMChan channel) >> wait thread
