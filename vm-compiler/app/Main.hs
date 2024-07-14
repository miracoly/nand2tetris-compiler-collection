module Main (main) where

import Data.Maybe (listToMaybe)
import Hack.VmCompiler (compile)
import System.Environment (getArgs)

main :: IO ()
main = do
  mArgs <- listToMaybe <$> getArgs
  raw <- maybe getContents readFile mArgs
  let compiled = compile raw
  case compiled of
    Left e -> print e
    Right asm -> putStrLn asm
