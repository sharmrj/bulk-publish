{-# LANGUAGE OverloadedStrings #-}

module Post
  ( post,
    previewList,
    publishList,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception.Safe
import Network.HTTP.Simple
import Utils

generateRequest :: String -> String -> String -> String -> Bool -> IO Request
generateRequest path owner repo ref p =
  parseRequest $
    "POST https://admin.hlx.page/"
      ++ b
      ++ "/"
      ++ owner
      ++ "/"
      ++ repo
      ++ "/"
      ++ ref
      ++ path
  where
    b = if p then "live" else "preview"

post :: String -> String -> String -> String -> Bool -> IO ()
post path owner repo ref p = do
  url <- generateRequest path owner repo ref p
  response <- tryAny $ (httpJSON url :: IO (Response ()))
  case response of
    Left e -> out $ path <> " Could not be " <> actioned <> " " <> show e
    Right resp -> do
      let statusCode = show $ getResponseStatusCode resp
      case statusCode of
        "200" -> out $ "Successfully " <> actioned <> " " <> path
        "404" -> out $ "404 " <> path <> " not found"
        "429" -> waitAMinuteAndTryAgain
        "503" -> waitAMinuteAndTryAgain -- admin apis are wierd
        _ -> out $ path <> " wasn't " <> actioned <> ": " <> statusCode
  where
    actioned = if p then "Published" else "Previewed"
    out m = do
      putStrLn m
      writeLog m
    waitAMinuteAndTryAgain = do
      out $ path <> " wasn't " <> actioned <> ". Trying again after a minute"
      threadDelay $ 60 * 1000000
      post path owner repo ref p

previewList :: [String] -> String -> String -> String -> IO ()
previewList = postList False

publishList :: [String] -> String -> String -> String -> IO ()
publishList = postList True

postList :: Bool -> [String] -> String -> String -> String -> IO ()
postList p paths owner repo ref =
  concurrentlyLimited 2 (map (\path -> post path owner repo ref p) paths) >>= mapM_ return
