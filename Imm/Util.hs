module Imm.Util where

import Data.Maybe

mapIOMaybe :: (a -> IO (Maybe b)) -> [a] -> IO [b]
mapIOMaybe f l = mapIOMaybe' f l []

mapIOMaybe' :: (a -> IO (Maybe b)) -> [a] -> [b] -> IO [b]
mapIOMaybe' _ [] acc    = return acc
mapIOMaybe' f (h:t) acc = do
  x <- f h
  mapIOMaybe' f t (catMaybes [x] ++ acc)
  
  