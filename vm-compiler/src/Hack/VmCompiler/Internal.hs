module Hack.VmCompiler.Internal (module Hack.VmCompiler.Internal) where

import Text.Parsec (ParseError, char, parse, many1, spaces, (<|>), try, many)
import Text.Parsec.String (GenParser, Parser)
import Text.Parsec.Char (letter, digit)

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
parseVmCommands = parse pCommands ""

pCommands :: Parser [VmCommand]
pCommands = many pCommand

pCommand :: Parser VmCommand
pCommand =  try pArithmetic <|> try pLogical <|> pPopPush

pPopPush :: Parser VmCommand
pPopPush = do
  command <- many1 letter
  spaces
  segment <- pSegment
  spaces
  index <- many1 digit
  _ <- eol
  case command of
    "push" -> return $ Push segment (read index)
    "pop" -> return $ Pop segment (read index)
    _ -> fail "Invalid VM command"

pArithmetic :: Parser VmCommand
pArithmetic = do
  command <- many1 letter
  _ <- eol
  case command of
    "add" -> return Add
    "sub" -> return Sub
    "neg" -> return Neg
    _ -> fail "Invalid VM command"

pLogical :: Parser VmCommand
pLogical = do
  command <- many1 letter
  _ <- eol
  case command of
    "eq" -> return Eq
    "gt" -> return Gt
    "lt" -> return Lt
    "and" -> return And
    "or" -> return Or
    "not" -> return Not
    _ -> fail "Invalid VM command"

pSegment :: Parser Segment
pSegment = do
  segment <- many1 letter
  case segment of
    "argument" -> return Argument
    "local" -> return Local
    "static" -> return Static
    "constant" -> return Constant
    "this" -> return This
    "that" -> return That
    "pointer" -> return Pointer
    "temp" -> return Temp
    _ -> fail "Invalid segment"

eol :: GenParser Char st Char
eol = char '\n'
