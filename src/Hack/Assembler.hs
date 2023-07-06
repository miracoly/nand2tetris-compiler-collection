module Hack.Assembler (parse, machineCode) where

import Data.Text as T (Text, filter, unlines)
import Hack.Assembler.Internal

-- | Parse text of Hack assembly instructions to binary representation of type
-- 'Hack.Assembler.Internal.Instruction'
parse :: Text -> Either ParseErrorDescription [Instruction]
parse = traverse parseInstruction . cleanUpCode

machineCode :: [Instruction] -> Text
machineCode = T.unlines . fmap (T.filter (/= ' ') . binary)
