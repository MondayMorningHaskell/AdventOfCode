{-# LANGUAGE OverloadedStrings #-}

module FetchInput where

import Control.Monad.Logger (MonadLogger, logDebugN, logErrorN)
import Conduit (MonadThrow)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Network.HTTP.Simple (parseRequest, addRequestHeader, getResponseBody, httpBS)
import System.Directory (doesFileExist, getFileSize)
import System.Environment (lookupEnv)

fetchInputToFile :: (MonadLogger m, MonadThrow m, MonadIO m) => Int -> Int -> FilePath -> m ()
fetchInputToFile year day filepath = do
  isCached <- liftIO $ checkFileExistsWithData filepath
  token' <- liftIO $ lookupEnv "AOC_TOKEN"
  case (isCached, token') of
    (True, _) -> logDebugN "Input is cached!"
    (False, Nothing) -> logErrorN "Not cached but didn't find session token!"
    (False, Just token) -> do
      let route = "https://adventofcode.com/" <> show year <> "/day/" <> show day <> "/input"
      req <- parseRequest route
      let req' = addRequestHeader "cookie" (pack token) req
      response <- getResponseBody <$> httpBS req'
      liftIO $ B.writeFile filepath response

checkFileExistsWithData :: FilePath -> IO Bool
checkFileExistsWithData fp = do
  exists <- doesFileExist fp
  if not exists
    then return False
    else do
      size <- getFileSize fp
      return $ size > 0
