module Main (main) where

import Control.Exception.Safe
import ParseExcel (extract)
import Post
import System.Environment

main :: IO ()
main = do
  process

process :: IO ()
process = do
  args <- tryAny getArgs
  case args of
    Left _ -> putStrLn "usage: bulk-publish [path to the import report excel file] [owner] [repo] [ref]"
    Right [importReportPath, owner, repo, ref] -> extractAndPublish importReportPath owner repo ref
    _ -> putStrLn "usage: bulk-publish [path to the import report excel file] [owner] [repo] [ref]"

extractAndPublish :: String -> String -> String -> String -> IO ()
extractAndPublish importReport owner repo ref = do
  paths <- extract importReport
  previewList paths owner repo ref
