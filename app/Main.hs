module Main (main) where

import Control.Monad (when)
import Options.Applicative
import ParseArgs
import ParseExcel (extract)
import Post
import Utils

main :: IO ()
main = do
  let opts =
        info (options <**> helper) $
          fullDesc
            <> header "Bulk Publish"
            <> progDesc "A CLI tool for bulk publishing/previewing Franklin pages"
  (Options file owner repo ref actions) <- execParser opts
  putStrLn "Getting paths"
  ps <- parseFile file
  case ps of
    Nothing -> putStrLn "Failed to parse file"
    Just paths -> putStrLn "Successfully Extracted Paths" >> process actions
      where
        process (Actions preview publish) = do
          when preview $ previewList paths owner repo ref
          when publish $ publishList paths owner repo ref
        process Delete = deletePaths paths owner repo ref
        process Unpublish = unpublishList paths owner repo ref

parseFile :: String -> IO (Maybe [String])
parseFile file =
  case getFileFormat file of
    TXT -> readFile file >>= (return . Just . lines)
    XLSX -> extract file
    Unsupported -> return Nothing
