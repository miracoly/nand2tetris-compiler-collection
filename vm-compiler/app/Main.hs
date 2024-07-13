module Main (main) where

import Hack.VmCompiler (parseVmCommand)

main :: IO ()
main = do
  let parsed = parseVmCommand text
  print parsed

text :: String
text = "Hello, world!\nSecond Line\n"
