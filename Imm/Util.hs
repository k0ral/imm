module Imm.Util where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Digest.Pure.MD5
import Data.Maybe

import Network.HaskellNet.SMTP
import Network.URI


mapIOMaybe :: (a -> IO (Maybe b)) -> [a] -> IO [b]
mapIOMaybe f l = mapIOMaybe' f l []

mapIOMaybe' :: (a -> IO (Maybe b)) -> [a] -> [b] -> IO [b]
mapIOMaybe' _ [] acc    = return acc
mapIOMaybe' f (h:t) acc = do
  x <- f h
  mapIOMaybe' f t (catMaybes [x] ++ acc)

codeToResponse :: Num a => a -> Response
codeToResponse 211 = SystemStatus
codeToResponse 214 = HelpMessage
codeToResponse 220 = ServiceReady
codeToResponse 221 = ServiceClosing
codeToResponse 250 = Ok
codeToResponse 251 = UserNotLocal
codeToResponse 252 = CannotVerify
codeToResponse 354 = StartMailInput
codeToResponse 421 = ServiceNotAvailable
codeToResponse 450 = MailboxUnavailable
codeToResponse 451 = ErrorInProcessing
codeToResponse 452 = InsufficientSystemStorage
codeToResponse 500 = SyntaxError
codeToResponse 501 = ParameterError
codeToResponse 502 = CommandNotImplemented
codeToResponse 503 = BadSequence
codeToResponse 504 = ParameterNotImplemented
codeToResponse 550 = MailboxUnavailableError
codeToResponse 551 = UserNotLocalError
codeToResponse 552 = ExceededStorage
codeToResponse 553 = MailboxNotAllowed
codeToResponse 554 = TransactionFailed
codeToResponse _   = Ok -- FIXME


uriToFilePath :: URI -> FilePath
uriToFilePath uri = case uriAuthority uri of
  Just auth -> uriRegName auth ++ "-" ++ (take 5 $ (show . md5 . BL.pack . show) uri)
  _         -> "no-name"       ++ "-" ++ ((show . md5 . BL.pack . show) uri)