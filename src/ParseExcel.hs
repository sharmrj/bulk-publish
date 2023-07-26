{-# LANGUAGE ImportQualifiedPost #-}

module ParseExcel (extract) where

import Codec.Archive.Zip
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B8
import Data.List (isInfixOf)
import Text.XML.Hexml

extract :: String -> IO [String]
extract importReport = do
  putStrLn "Getting paths from Import Report"
  s <- mkEntrySelector "xl/sharedStrings.xml"
  bs <- withArchive importReport (getEntry s)
  let paths = getPaths . parse $ bs
  putStrLn "Successfully extracted paths"
  return paths

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
