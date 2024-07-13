module Main (main) where

import Data.Maybe (listToMaybe)
import Hack.VmCompiler (parseVmLines)
import System.Environment (getArgs)

main :: IO ()
main = do
  mArgs <- listToMaybe <$> getArgs
  raw <- maybe getContents readFile mArgs
  let parsed = parseVmLines raw
  print raw
  print parsed
