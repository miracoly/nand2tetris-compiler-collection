module Hack.VmCompiler.Internal (module Hack.VmCompiler.Internal) where

import Data.Char (isSpace)
import Text.Parsec
  ( char,
    many,
    many1,
    noneOf,
    spaces,
    try,
    (<|>),
  )
import Text.Parsec.Char (digit, letter)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (parse)
import Text.Parsec.String (Parser)

translateVmLines :: [VmLine] -> String
translateVmLines = unlines . (=<<) translateVmLine

translateVmLine :: VmLine -> [String]
translateVmLine l =
  case l of
    Command c -> translateVmCommand c
    Comment c -> ["// " ++ c]

-- | Translates a VM command to a list of assembly commands.
-- push static i -> *SP = filename.i; SP++;
-- pop static i -> SP--; filename.i = *SP;
translateVmCommand :: VmCommand -> [String]
translateVmCommand c =
  case c of
    Push seg i -> translatePush seg i
    Pop seg i -> translatePop seg i
    Add -> translateAdd
    Sub -> translateSub
    Neg -> translateNeg
    Eq -> translateEq
    Gt -> translateGt
    Lt -> translateLt
    And -> translateAnd
    Or -> translateOr
    Not -> undefined

translateAdd :: [String]
translateAdd =
  [ "@SP",
    "M=M-1",
    "A=M",
    "D=M",
    "@SP",
    "M=M-1",
    "A=M",
    "M=D+M",
    "@SP",
    "M=M+1"
  ]

translateSub :: [String]
translateSub =
  [ "@SP",
    "M=M-1",
    "A=M",
    "D=M",
    "@SP",
    "M=M-1",
    "A=M",
    "M=M-D",
    "@SP",
    "M=M+1"
  ]

translateNeg :: [String]
translateNeg =
  [ "@SP",
    "M=M-1",
    "A=M",
    "M=-M",
    "@SP",
    "M=M+1"
  ]

translateEq :: [String]
translateEq =
  [ "@SP",
    "AM=M-1",
    "D=M",
    "@SP",
    "AM=M-1",
    "@EQUAL",
    "D;JEQ",
    "@SP",
    "A=M",
    "M=0",
    "@END_EQ",
    "0;JMP",
    "(EQUAL)",
    "@SP",
    "A=M",
    "M=-1"
  ]

translateGt :: [String]
translateGt =
  [ "@SP",
    "AM=M-1",
    "D=M",
    "@SP",
    "AM=M-1",
    "D=M-D",
    "@GREATER",
    "D;JGT",
    "@SP",
    "A=M",
    "M=0",
    "@END_GT",
    "0;JMP",
    "(GREATER)",
    "@SP",
    "A=M",
    "M=-1",
    "(END_GT)",
    "@SP",
    "M=M+1"
  ]

translateLt :: [String]
translateLt =
  [ "@SP",
    "AM=M-1",
    "D=M",
    "@SP",
    "AM=M-1",
    "D=M-D",
    "@LESS",
    "D;JLT",
    "@SP",
    "A=M",
    "M=0",
    "@END_LT",
    "0;JMP",
    "(LESS)",
    "@SP",
    "A=M",
    "M=-1",
    "(END_LT)",
    "@SP",
    "M=M+1"
  ]

translateAnd :: [String]
translateAnd =
  [ "@SP",
    "AM=M-1",
    "D=M",
    "@SP",
    "AM=M-1",
    "M=D&M",
    "@SP",
    "M=M+1"
  ]

translateOr :: [String]
translateOr =
  [ "@SP",
    "AM=M-1",
    "D=M",
    "@SP",
    "AM=M-1",
    "M=D|M",
    "@SP",
    "M=M+1"
  ]

translateNot :: [String]
translateNot =
  [ "@SP",
    "AM=M-1",
    "M=!M",
    "@SP",
    "M=M+1"
  ]

translatePop :: VmSegment -> Int -> [String]
translatePop seg i =
  case seg of
    Local -> translatePopLocal i
    Argument -> translatePopArgument i
    This -> translatePopThis i
    That -> translatePopThat i
    Temp -> translatePopTemp i
    Pointer -> translatePopPointer i
    _ -> undefined

translatePopLocal :: Int -> [String]
translatePopLocal = translatePopSeg Local

translatePopArgument :: Int -> [String]
translatePopArgument = translatePopSeg Argument

translatePopThis :: Int -> [String]
translatePopThis = translatePopSeg This

translatePopThat :: Int -> [String]
translatePopThat = translatePopSeg That

translatePopTemp :: Int -> [String]
translatePopTemp i =
  [ "@" <> show i,
    "D=A",
    "@5",
    "D=D+A",
    "@R13",
    "M=D",
    "@SP",
    "M=M-1",
    "A=M",
    "D=M",
    "@R13",
    "A=M",
    "M=D"
  ]

translatePopPointer :: Int -> [String]
translatePopPointer i =
  [ "@SP",
    "M=M-1",
    "A=M",
    "D=M",
    "@" <> case i of
      0 -> "THIS"
      1 -> "THAT"
      _ -> error "Invalid pointer index",
    "M=D"
  ]

translatePopSeg :: VmSegment -> Int -> [String]
translatePopSeg seg i =
  case seg of
    s
      | s `elem` [Local, Argument, This, That] ->
          [ "@" <> show i,
            "D=A",
            "@" <> translateSeg seg i,
            "D=D+M",
            "@R13",
            "M=D",
            "@SP",
            "M=M-1",
            "A=M",
            "D=M",
            "@R13",
            "A=M",
            "M=D"
          ]
    _ -> error "Segment not supported by this function."

translatePush :: VmSegment -> Int -> [String]
translatePush seg i =
  case seg of
    Constant -> translatePushConstant i
    Local -> translatePushLocal i
    Argument -> translatePushArgument i
    This -> translatePushThis i
    That -> translatePushThat i
    Temp -> translatePushTemp i
    Pointer -> translatePushPointer i
    _ -> undefined

translatePushLocal :: Int -> [String]
translatePushLocal = translatePushSeg Local

translatePushArgument :: Int -> [String]
translatePushArgument = translatePushSeg Argument

translatePushThis :: Int -> [String]
translatePushThis = translatePushSeg This

translatePushThat :: Int -> [String]
translatePushThat = translatePushSeg That

translatePushPointer :: Int -> [String]
translatePushPointer i =
  [ "@" <> case i of
      0 -> "THIS"
      1 -> "THAT"
      _ -> error "Invalid pointer index",
    "D=M",
    "@SP",
    "A=M",
    "M=D",
    "@SP",
    "M=M+1"
  ]

translatePushSeg :: VmSegment -> Int -> [String]
translatePushSeg seg i =
  case seg of
    s
      | s `elem` [Local, Argument, This, That] ->
          [ "@" <> show i,
            "D=A",
            "@" <> translateSeg seg i,
            "A=D+M",
            "D=M",
            "@SP",
            "A=M",
            "M=D",
            "@SP",
            "M=M+1"
          ]
    _ -> error "Segment not supported by this function."

translatePushTemp :: Int -> [String]
translatePushTemp i =
  [ "@" <> show i,
    "D=A",
    "@5",
    "A=D+A",
    "D=M",
    "@SP",
    "A=M",
    "M=D",
    "@SP",
    "M=M+1"
  ]

translatePushConstant :: Int -> [String]
translatePushConstant i =
  [ "@" <> show i,
    "D=A",
    "@SP",
    "A=M",
    "M=D",
    "@SP",
    "M=M+1"
  ]

-- | TODO: partial function
translateSeg :: VmSegment -> Int -> String
translateSeg seg i =
  case seg of
    Constant -> "SP"
    Local -> "LCL"
    Argument -> "ARG"
    This -> "THIS"
    That -> "THAT"
    Temp
      | i > 7 || i < 0 -> error "Invalid temp index"
      | otherwise -> "R" <> show (5 + i)
    Pointer
      | i == 0 -> "THIS"
      | i == 1 -> "THAT"
      | otherwise -> error "Invalid pointer index"
    Static -> undefined

-- | Represents a line of VM code.
data VmLine
  = Command VmCommand
  | Comment String
  deriving (Show, Eq)

-- | Represents a VM command.
data VmCommand
  = Push VmSegment Int
  | Pop VmSegment Int
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
data VmSegment
  = Constant
  | Local
  | Argument
  | This
  | That
  | Temp
  | Pointer
  | Static
  deriving (Show, Eq)

-- | Parses a list of VM commands including comments.
parseVmLines :: String -> Either ParseError [VmLine]
parseVmLines = parse pLines ""

pLines :: Parser [VmLine]
pLines = many pLine

pLine :: Parser VmLine
pLine = try (Command <$> pCommand) <|> (Comment <$> pComment)

pComment :: Parser String
pComment = do
  _ <- char '/'
  _ <- char '/'
  comment <- many1 $ noneOf "\n"
  _ <- eol
  return $ trim comment

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

pCommand :: Parser VmCommand
pCommand = try pArithmetic <|> try pLogical <|> pPopPush

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

pSegment :: Parser VmSegment
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

eol :: Parser String
eol = many $ char '\n' <|> char '\r'
