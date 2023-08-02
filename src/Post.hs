{-# LANGUAGE OverloadedStrings #-}

-- TODO: Refactor this file

module Post
  ( post,
    previewList,
    publishList,
    unpublishList,
    deletePaths,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception.Safe
import Data.ByteString.Char8 (unpack)
import Network.HTTP.Simple
import Utils

constructURL :: String -> String -> String -> Bool -> String
constructURL owner repo ref p =
  "https://admin.hlx.page/"
    <> b
    <> "/"
    <> owner
    <> "/"
    <> repo
    <> "/"
    <> ref
  where
    b = if p then "live" else "preview"

post :: String -> String -> Bool -> IO ()
post path url p = do
  request <- parseRequest $ "POST " <> url <> path
  response <- tryAny $ httpNoBody request
  case response of
    Left e -> out $ path <> " Could not be " <> actioned <> " " <> show e
    Right resp -> do
      let statusCode = show $ getResponseStatusCode resp
      case statusCode of
        "200" -> out $ "Successfully " <> actioned <> " " <> path
        "404" -> out $ "404 " <> path <> " not found"
        "429" -> waitAMinuteAndTryAgain
        "503" -> waitAMinuteAndTryAgain -- admin apis are weird
        _ -> out $ path <> " wasn't " <> actioned <> ": " <> statusCode
  where
    actioned = if p then "Published" else "Previewed"
    out m = do
      putStrLn m
      writeLog m
    waitAMinuteAndTryAgain = do
      out $ path <> " wasn't " <> actioned <> ". Trying again after a minute"
      threadDelay $ 60 * 1000000
      post path url p

delete :: String -> String -> Bool -> IO ()
delete path url p = do
  request <- parseRequest $ "DELETE " <> url <> path
  response <- tryAny $ httpNoBody request
  case response of
    Left e -> out $ subject <> path <> " Could not be " <> verb <> " " <> show e
    Right resp -> do
      let statusCode = show $ getResponseStatusCode resp
      case statusCode of
        "204" -> out $ subject <> path <> " Successfully " <> verb <> " "
        "404" -> out $ "404 " <> path <> " not found"
        "429" -> waitAMinuteAndTryAgain
        "503" -> waitAMinuteAndTryAgain -- admin apis are weird
        "403" ->
          let xerror = safeHead $ getResponseHeader "x-error" resp
           in case xerror of
                Just x -> out $ path <> ": " <> unpack x
                Nothing -> out $ subject <> path <> " could not be " <> verb <> "(most likely because the resource still exists)"
        _ -> out $ subject <> path <> " Could not be " <> verb <> ": " <> statusCode
  where
    verb = if p then "Un-Published" else "Deleted"
    subject = if p then "" else "Preview of "
    out m = do
      putStrLn m
      writeLog m
    waitAMinuteAndTryAgain = do
      out $ path <> " wasn't " <> verb <> ". Trying again after a minute"
      threadDelay $ 60 * 1000000
      post path url p
    safeHead [] = Nothing
    safeHead (x : _) = Just x

previewList :: [String] -> String -> String -> String -> IO ()
previewList paths owner repo ref = postList False paths (constructURL owner repo ref False)

publishList :: [String] -> String -> String -> String -> IO ()
publishList paths owner repo ref = postList True paths (constructURL owner repo ref True)

postList :: Bool -> [String] -> String -> IO ()
postList p paths url =
  concurrentlyLimited 2 (map (\path -> post path url p) paths) >>= mapM_ return

unpublishList :: [String] -> String -> String -> String -> IO ()
unpublishList paths owner repo ref = deleteList True paths (constructURL owner repo ref True)

unpreviewList :: [String] -> String -> String -> String -> IO ()
unpreviewList paths owner repo ref = deleteList False paths (constructURL owner repo ref False)

deletePaths :: [String] -> String -> String -> String -> IO ()
deletePaths paths owner repo ref = do
  unpreviewList paths owner repo ref
  unpublishList paths owner repo ref

deleteList :: Bool -> [String] -> String -> IO ()
deleteList p paths url =
  concurrentlyLimited 2 (map (\path -> delete path url p) paths) >>= mapM_ return
