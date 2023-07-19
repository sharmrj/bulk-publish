{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Codec.Archive.Zip
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Exception.Safe
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B8
import Data.List (delete, isInfixOf, sortBy)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)
import Network.HTTP.Simple
import System.Environment
import Text.XML.Hexml

main :: IO [()]
main = do
  [importReportPath, owner, repo, ref] <- getArgs
  putStrLn "Getting paths from Import Report"
  s <- mkEntrySelector "xl/sharedStrings.xml"
  bs <- withArchive importReportPath (getEntry s)
  let paths = getPaths . parse $ bs
  putStrLn "Successfully extracted paths"
  concurrentlyLimited 2 (map (\path -> makePOSTCalls [path, owner, repo, ref]) paths)

concurrentlyLimited :: Integer -> [IO a] -> IO [a]
concurrentlyLimited n tasks = conc n (zip [0 ..] tasks) [] []
  where
    -- If there are no todos or ongoing tasks
    conc _ [] [] results = return . map snd $ sortBy (comparing fst) results
    -- If there are no resources left, wait for a resource to become available
    conc 0 todo ongoing results = do
      (task, result) <- waitAny ongoing
      conc 1 todo (delete task ongoing) (result : results)
    -- if there are no tasks left
    conc _ [] ongoing results = conc 0 [] ongoing results
    -- Add a new task to the pool (should a resource be available)
    conc num ((i, x) : xs) ongoing results = do
      t <- async $ (i,) <$> x
      conc (num - 1) xs (t : ongoing) results

makePOSTCalls :: [String] -> IO ()
makePOSTCalls args = concurrently_ (post preview " Previewed ") (post publish " Published ")
  where
    post f mes = do
      resp <- tryAny $ f args
      case resp of
        Left e -> putStrLn $ "Failed to" ++ take ((length mes) - 3) mes ++ " " ++ args !! 0 ++ "\n" ++ show e
        Right r -> handleBody f r mes
    handleBody f resp mes = do
      let statusCode = show $ getResponseStatusCode resp
      case statusCode of
        "200" -> putStrLn $ "Successfully" ++ mes ++ (args !! 0)
        "404" -> putStrLn $ "404 " ++ args !! 0 ++ " not found"
        "503" -> waitAMinuteAndTryAgain f mes
        _ -> print $ statusCode ++ " Failed to" ++ take ((length mes) - 3) mes ++ " " ++ args !! 0
    waitAMinuteAndTryAgain f mes = do
      putStrLn $ args !! 0 ++ " wasn't" ++ mes ++ ". Trying again after a minute"
      threadDelay $ 60 * 1000000
      post f mes

getPaths :: Either ByteString Node -> [String]
getPaths (Left _) = ["Failed to parse Excel file"]
getPaths (Right node) = filter isPath $ map (getNestedContent) strings
  where
    strings = children $ (children node) !! 1
    isPath str = (take 1 str) == "/" && (not $ isInfixOf ".docx" str)

getNestedContent :: Node -> String
getNestedContent node =
  case c of
    Left bs -> B8.unpack bs
    Right n -> getNestedContent n
  where
    c = if (length cs > 0) then cs !! 0 else (Left $ B8.pack "")
    cs = contents node

generateRequest :: [String] -> Bool -> IO Request
generateRequest [path, owner, repo, ref] p =
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

preview :: [String] -> IO (Response (Either JSONException ()))
preview args = generateRequest args False >>= httpJSONEither

publish :: [String] -> IO (Response (Either JSONException ()))
publish args = generateRequest args True >>= httpJSONEither
