module Main (main) where

import Data.Maybe (listToMaybe, fromMaybe)
import Hack.VmCompiler (compile)
import System.Environment (getArgs)
import Control.Monad.Reader (runReader)

main :: IO ()
main = do
  mArgs <- listToMaybe <$> getArgs
  raw <- maybe getContents readFile mArgs
  let filename = fromMaybe "Stdin" mArgs
  let compiled = runReader (compile raw) filename
  case compiled of
    Left e -> print e
    Right asm -> putStrLn asm
