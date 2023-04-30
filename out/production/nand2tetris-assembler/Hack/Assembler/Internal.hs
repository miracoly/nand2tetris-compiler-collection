{-# LANGUAGE OverloadedStrings #-}

module Hack.Assembler.Internal (module Hack.Assembler.Internal) where

import Data.Maybe (fromMaybe)
import Data.Text as T (Text, filter, lines, stripSuffix, unlines, take, strip)
import Prelude as P hiding (dropWhile, lines, unlines, length, take)

-- | Remove whitespaces, empty lines and comments.
cleanUpCode :: Text -> Text
cleanUpCode =
  fromMaybe ""
    . stripSuffix "\n"
    . unlines
    . P.filter (/= "")
    . P.filter (/= "")
    . P.filter (/= "\n")
    . T.lines
    . T.filter (/= ' ')

isComment :: Text -> Bool
isComment = (== "//") . take 2 . strip