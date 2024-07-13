module Main (main) where

import Hack.VmCompiler (parseVmLines)

main :: IO ()
main = do
  let parsed = parseVmLines text
  print parsed

text :: String
text = "push constant 7\npush constant 8\nadd\n"
