module Hack.VmCompiler (module Hack.VmCompiler) where

import Text.Parsec (ParseError, char, anyToken, manyTill, many, parse)
import Text.Parsec.String (GenParser)

-- | Represents a VM command.
data VmCommand
  = Push Segment Int
  | Pop Segment Int
  | Add
  | Sub
  | Neg
  | Eq
  | Gt
  | Lt
  | And
  | Or
  | Not
  deriving (Show)

-- | Represents a segment of the VM memory used by push and pop commands.
data Segment
  = Constant
  | Local
  | Argument
  | This
  | That
  | Temp
  | Pointer
  | Static
  deriving (Show)

parseVmCommand :: String -> Either ParseError [String]
parseVmCommand = parse pLines "(unknown)"

pLines :: GenParser Char st [String]
pLines = many pLine

pLine :: GenParser Char st String
pLine = manyTill anyToken eol

eol :: GenParser Char st Char
eol = char '\n'
