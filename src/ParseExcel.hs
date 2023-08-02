{-# LANGUAGE ImportQualifiedPost #-}

module ParseExcel (extract) where

import Codec.Archive.Zip
import Data.ByteString.Char8 qualified as B8
import Data.List (isInfixOf)
import Text.XML.Hexml

extract :: String -> IO (Maybe [String])
extract importReport = do
  s <- mkEntrySelector "xl/sharedStrings.xml"
  bs <- withArchive importReport (getEntry s)
  case parse bs of
    Left _ -> return Nothing
    Right node -> return . Just . getPaths $ node

getPaths :: Node -> [String]
getPaths node = filter isPath $ map getNestedContent strings
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
