module Hack.VmCompiler (compile) where

import Hack.VmCompiler.Internal
import Text.Parsec (ParseError)

compile :: String -> Either ParseError String
compile = fmap translateVmLines . parseVmLines

