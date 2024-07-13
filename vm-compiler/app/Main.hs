module Main (main) where

import Hack.VmCompiler (parseVmCommands)

main :: IO ()
main = do
  let parsed = parseVmCommands text
  print parsed

text :: String
text = "push constant 7\npush constant 8\nadd\n"
