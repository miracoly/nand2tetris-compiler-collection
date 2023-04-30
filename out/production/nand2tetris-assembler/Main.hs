{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text.IO as T (getContents, putStr)

main :: IO ()
main = do
  content <- T.getContents
  T.putStr content
