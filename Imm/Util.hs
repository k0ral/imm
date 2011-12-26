module Imm.Util where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Digest.Pure.MD5
import Data.Maybe

import Network.URI


mapIOMaybe :: (a -> IO (Maybe b)) -> [a] -> IO [b]
mapIOMaybe f l = mapIOMaybe' f l []

mapIOMaybe' :: (a -> IO (Maybe b)) -> [a] -> [b] -> IO [b]
mapIOMaybe' _ [] acc    = return acc
mapIOMaybe' f (h:t) acc = do
  x <- f h
  mapIOMaybe' f t (catMaybes [x] ++ acc)

uriToFilePath :: URI -> FilePath
uriToFilePath uri = case uriAuthority uri of
  Just auth -> uriRegName auth ++ "-" ++ (take 8 $ (show . md5 . BL.pack . show) uri)
  _         -> "no-name"       ++ "-" ++ ((show . md5 . BL.pack . show) uri)
