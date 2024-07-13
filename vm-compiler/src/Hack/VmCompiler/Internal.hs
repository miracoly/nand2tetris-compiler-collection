module Hack.VmCompiler.Internal (module Hack.VmCompiler.Internal) where

import Text.Parsec (ParseError, anyToken, char, many, manyTill, parse)
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
  deriving (Show, Eq)

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
  deriving (Show, Eq)

parseVmCommands :: String -> Either ParseError [VmCommand]
parseVmCommands = parse pVmCommands ""

pVmCommands :: GenParser Char st [VmCommand]
pVmCommands = many pVmCommand

pVmCommand :: GenParser Char st VmCommand
pVmCommand = do
  command <- manyTill anyToken (char ' ')
  case command of
    "push" -> do
      segment <- pVmSegment
      Push segment <$> pIndex
    "pop" -> do
      segment <- pVmSegment
      Pop segment <$> pIndex
    "add" -> return Add
    "sub" -> return Sub
    "neg" -> return Neg
    "eq" -> return Eq
    "gt" -> return Gt
    "lt" -> return Lt
    "and" -> return And
    "or" -> return Or
    "not" -> return Not
    _ -> fail "Invalid VM command"

pVmSegment :: GenParser Char st Segment
pVmSegment = do
  segment <- manyTill anyToken (char ' ')
  case segment of
    "constant" -> return Constant
    "local" -> return Local
    "argument" -> return Argument
    "this" -> return This
    "that" -> return That
    "temp" -> return Temp
    "pointer" -> return Pointer
    "static" -> return Static
    _ -> fail "Invalid VM segment"

pIndex :: GenParser Char st Int
pIndex = do
  index <- manyTill anyToken eol
  case reads index of
    [(i, "")] -> return i
    _ -> fail "Invalid index"

pLines :: GenParser Char st [String]
pLines = many pLine

pLine :: GenParser Char st String
pLine = manyTill anyToken eol

eol :: GenParser Char st Char
eol = char '\n'
