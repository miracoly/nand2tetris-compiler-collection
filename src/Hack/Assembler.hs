module Hack.Assembler (compile, parse, machineCode) where

import Data.Text as T (Text, filter, unlines)
import Hack.Assembler.Internal

-- | Compiles `Data.Text` of Hack assembly instructions into binary representation defined by the
-- Hack architecture.
compile :: Text -> Either ParseErrorDescription Text
compile = fmap machineCode . parse

-- | Parse text of Hack assembly instructions list of 'Hack.Assembler.Internal.Instruction'
parse :: Text -> Either ParseErrorDescription [Instruction]
parse = traverse parseInstruction . convertSymbols . cleanUpCode

-- | Transforms list of 'Hack.Assembler.Internal.Instruction' into binary representation.
machineCode :: [Instruction] -> Text
machineCode = T.unlines . fmap (T.filter (/= ' ') . binary)
