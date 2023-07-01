module Hack.Assembler (parse) where

import Data.Text as T (Text, lines)
import Hack.Assembler.Internal

-- | Parse text of Hack assembly instructions to binary representation of type
-- 'Hack.Assembler.Internal.Instruction'
parse :: Text -> Either ParseErrorDescription [Instruction]
parse = traverse parseInstruction . T.lines . cleanUpCode

machineCode :: [Instruction] -> Text
machineCode = undefined
--  where
    
