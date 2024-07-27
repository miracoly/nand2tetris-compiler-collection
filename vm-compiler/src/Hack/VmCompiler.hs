module Hack.VmCompiler (compile) where

import Hack.VmCompiler.Internal
import Text.Parsec (ParseError)
import Control.Monad.Reader (Reader)

compile :: String -> Reader FilePath (Either ParseError String)
compile = traverse translateVmLines . parseVmLines

