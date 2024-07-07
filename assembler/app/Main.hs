{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Maybe (listToMaybe)
import Data.Text (Text, pack)
import qualified Data.Text.IO as Tio (readFile, writeFile, getContents, putStr)
import Hack.Assembler (compile)
import System.Environment (getArgs)

main :: IO ()
main =
  getArgs
    >>= maybe
      fromStdin
      ((=<<) (either putStrLn handleSuccess) . fmap compile . Tio.readFile)
      . listToMaybe
      
fromStdin :: IO ()
fromStdin = Tio.getContents >>= (Tio.putStr . either pack id . compile)

handleSuccess :: Text -> IO ()
handleSuccess = (>>) (putStrLn "Success. Wrote to output.hack") . Tio.writeFile "output.hack"
