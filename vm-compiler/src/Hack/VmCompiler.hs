module Hack.VmCompiler (parseVmLines) where

import Hack.VmCompiler.Internal
import Text.Parsec (ParseError, parse)

-- | Parses a list of VM commands including comments.
parseVmLines :: String -> Either ParseError [VmLine]
parseVmLines = parse pLines ""
