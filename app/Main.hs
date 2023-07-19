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
import Data.Ord (comparing)
import Network.HTTP.Simple
import System.Environment
import Text.XML.Hexml

main :: IO ()
main = do
  process

process :: IO ()
process = do
  args <- tryAny getArgs
  case args of
    Left _ -> putStrLn "usage: bulk-publish [path to the import report excel file] [owner] [repo] [ref]"
    Right [importReportPath, owner, repo, ref] -> extractAndPublish importReportPath owner repo ref >>= mapM_ return
    _ -> putStrLn "usage: bulk-publish [path to the import report excel file] [owner] [repo] [ref]"

extractAndPublish :: String -> String -> String -> String -> IO [()]
extractAndPublish importReportPath owner repo ref = do
  putStrLn "Getting paths from Import Report"
  s <- mkEntrySelector "xl/sharedStrings.xml"
  bs <- withArchive importReportPath (getEntry s)
  let paths = getPaths . parse $ bs
  putStrLn "Successfully extracted paths"
  concurrentlyLimited 2 (map (\path -> makePOSTCalls path owner repo ref) paths)

concurrentlyLimited :: Integer -> [IO a] -> IO [a]
concurrentlyLimited n tasks = conc n (zip [0 ..] tasks) [] []
  where
    conc :: Integer -> [(Integer, IO a)] -> [Async (Integer, a)] -> [(Integer, a)] -> IO [a]
    -- If there are no todos or ongoing tasks, give back the results (sorted to preserve order)
    conc _ [] [] results = return . map snd $ sortBy (comparing fst) results
    -- If there are no resources left, wait for a resource to become available
    conc 0 todo ongoing results = do
      (task, result) <- waitAny ongoing
      conc 1 todo (delete task ongoing) (result : results)
    -- if there are no tasks left
    conc _ [] ongoing results = conc 0 [] ongoing results
    -- Add a new task to the pool (should a resource be available)
    conc num ((i, x) : xs) ongoing results = withAsync ((i,) <$> x) (\task -> conc (num - 1) xs (task : ongoing) results)

makePOSTCalls :: String -> String -> String -> String -> IO ()
makePOSTCalls path owner repo ref = concurrently_ (post preview " Previewed ") (post publish " Published ")
  where
    post f mes = do
      resp <- tryAny $ f path owner repo ref
      case resp of
        Left e -> putStrLn $ "Failed to" ++ take (length mes - 3) mes ++ " " ++ path ++ "\n" ++ show e
        Right r -> handleBody f r mes
    handleBody f resp mes = do
      let statusCode = show $ getResponseStatusCode resp
      case statusCode of
        "200" -> putStrLn $ "Successfully" ++ mes ++ path
        "404" -> putStrLn $ "404 " ++ path ++ " not found"
        "503" -> waitAMinuteAndTryAgain f mes
        _ -> print $ statusCode ++ " Failed to" ++ take (length mes - 3) mes ++ " " ++ path
    waitAMinuteAndTryAgain f mes = do
      putStrLn $ path ++ " wasn't" ++ mes ++ ". Trying again after a minute"
      threadDelay $ 60 * 1000000
      post f mes

getPaths :: Either ByteString Node -> [String]
getPaths (Left _) = ["Failed to parse Excel file"]
getPaths (Right node) = filter isPath $ map getNestedContent strings
  where
    strings = children $ children node !! 1
    isPath str = take 1 str == "/" && not (".docx" `isInfixOf` str)

getNestedContent :: Node -> String
getNestedContent node =
  case c of
    Left bs -> B8.unpack bs
    Right n -> getNestedContent n
  where
    c = if not (null cs) then head cs else Left $ B8.pack ""
    cs = contents node

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

preview :: String -> String -> String -> String -> IO (Response (Either JSONException ()))
preview path owner repo ref = generateRequest path owner repo ref False >>= httpJSONEither

publish :: String -> String -> String -> String -> IO (Response (Either JSONException ()))
publish path owner repo ref = generateRequest path owner repo ref True >>= httpJSONEither
