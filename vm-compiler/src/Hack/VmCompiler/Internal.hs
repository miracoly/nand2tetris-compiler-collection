{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Hack.VmCompiler.Internal (module Hack.VmCompiler.Internal) where

import Control.Monad.Reader (MonadReader (ask), Reader, mapReader, runReader)
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

translateVmLines :: [VmLine] -> Reader FilePath String
translateVmLines = fmap unlines . go 0
  where
    go :: Int -> [VmLine] -> Reader FilePath [String]
    go _ [] = return []
    go i (l : ls) =
      let f (cs, i') = mapReader (cs ++) (go i' ls)
       in translateVmLine i l >>= f

translateVmLine :: Int -> VmLine -> Reader FilePath ([String], Int)
translateVmLine i l = do
  case l of
    Command c -> translateVmCommand i c
    Comment c -> return (["// " ++ c], i)

-- | Translates a VM command to a list of assembly commands.
-- pop static i -> SP--; filename.i = *SP;
translateVmCommand :: Int -> VmCommand -> Reader FilePath ([String], Int)
translateVmCommand i c = do
  fp <- ask
  let run r = runReader (mapReader (,i) r) fp
  return $ case c of
    Push seg addr -> run $ translatePush seg addr
    Pop seg addr -> run $ translatePop seg addr
    Add -> (translateAdd, i)
    Sub -> (translateSub, i)
    Neg -> (translateNeg, i)
    Eq -> (translateEq i, i + 1)
    Gt -> (translateGt i, i + 1)
    Lt -> (translateLt i, i + 1)
    And -> (translateAnd, i)
    Or -> (translateOr, i)
    Not -> (translateNot, i)

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

translateEq :: Int -> [String]
translateEq i =
  let i' = show i
   in [ "@SP",
        "AM=M-1",
        "D=M",
        "@SP",
        "AM=M-1",
        "D=M-D",
        "@EQUAL_" <> i',
        "D;JEQ",
        "@SP",
        "A=M",
        "M=0",
        "@END_EQ_" <> i',
        "0;JMP",
        "(EQUAL_" <> i' <> ")",
        "@SP",
        "A=M",
        "M=-1",
        "(END_EQ_" <> i' <> ")",
        "@SP",
        "M=M+1"
      ]

translateGt :: Int -> [String]
translateGt i =
  let i' = show i
   in [ "@SP",
        "AM=M-1",
        "D=M",
        "@SP",
        "AM=M-1",
        "D=M-D",
        "@GREATER_" <> i',
        "D;JGT",
        "@SP",
        "A=M",
        "M=0",
        "@END_GT_" <> i',
        "0;JMP",
        "(GREATER_" <> i' <> ")",
        "@SP",
        "A=M",
        "M=-1",
        "(END_GT_" <> i' <> ")",
        "@SP",
        "M=M+1"
      ]

translateLt :: Int -> [String]
translateLt i =
  let i' = show i
   in [ "@SP",
        "AM=M-1",
        "D=M",
        "@SP",
        "AM=M-1",
        "D=M-D",
        "@LESS_" <> i',
        "D;JLT",
        "@SP",
        "A=M",
        "M=0",
        "@END_LT_" <> i',
        "0;JMP",
        "(LESS_" <> i' <> ")",
        "@SP",
        "A=M",
        "M=-1",
        "(END_LT_" <> i' <> ")",
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

translatePop :: VmSegment -> Int -> Reader FilePath [String]
translatePop seg i = do
  fp <- ask
  return $ case seg of
    Local -> translatePopLocal i
    Argument -> translatePopArgument i
    This -> translatePopThis i
    That -> translatePopThat i
    Temp -> translatePopTemp i
    Pointer -> translatePopPointer i
    Static -> translatePopStatic fp i
    Constant -> error "Cannot pop to constant segment"

translatePopLocal :: Int -> [String]
translatePopLocal = translatePopSeg Local

translatePopArgument :: Int -> [String]
translatePopArgument = translatePopSeg Argument

translatePopThis :: Int -> [String]
translatePopThis = translatePopSeg This

translatePopThat :: Int -> [String]
translatePopThat = translatePopSeg That

translatePopStatic :: FilePath -> Int -> [String]
translatePopStatic filename i =
  [ "@SP",
    "AM=M-1",
    "D=M",
    "@" <> filename <> "." <> show i,
    "M=D"
  ]

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

translatePush :: VmSegment -> Int -> Reader FilePath [String]
translatePush seg i = do
  fp <- ask
  return $ case seg of
    Constant -> translatePushConstant i
    Local -> translatePushLocal i
    Argument -> translatePushArgument i
    This -> translatePushThis i
    That -> translatePushThat i
    Temp -> translatePushTemp i
    Pointer -> translatePushPointer i
    Static -> translatePushStatic fp i

translatePushLocal :: Int -> [String]
translatePushLocal = translatePushSeg Local

translatePushArgument :: Int -> [String]
translatePushArgument = translatePushSeg Argument

translatePushThis :: Int -> [String]
translatePushThis = translatePushSeg This

translatePushThat :: Int -> [String]
translatePushThat = translatePushSeg That

translatePushStatic :: FilePath -> Int -> [String]
translatePushStatic filename i =
  [ "@" <> filename <> "." <> show i,
    "D=M",
    "@SP",
    "A=M",
    "M=D",
    "@SP",
    "M=M+1"
  ]

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
