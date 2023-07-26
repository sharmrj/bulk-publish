module Main (main) where

import Control.Exception.Safe
import Options.Applicative
import ParseArgs
import ParseExcel (extract)
import Post

main :: IO ()
main = do
  process

process :: IO ()
process = do
  let opts =
        info (options <**> helper) $
          fullDesc
            <> header "Bulk Publish"
            <> progDesc "A CLI tool for bulk publishing/previewing Franklin pages"
  (Options importReport owner repo ref preview publish) <- execParser opts
  paths <- extract importReport
  if preview then previewList paths owner repo ref else pure ()
  if publish then publishList paths owner repo ref else pure ()
