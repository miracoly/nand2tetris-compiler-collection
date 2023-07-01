{-# LANGUAGE OverloadedStrings #-}

module Hack.Assembler.Internal (module Hack.Assembler.Internal) where

import Control.Applicative (ZipList (..), liftA2, (<|>))
import Control.Monad (join)
import Data.List as L (head, length)
import Data.Maybe (fromMaybe)
import Data.Text as T (Text, elem, filter, head, length, lines, null, split, strip, stripSuffix, tail, take, unlines, unpack, pack)
import Data.Text.Read (Reader, decimal)
import Numeric (showBin)
import Prelude as P hiding (dropWhile, length, lines, null, span, take, unlines)
import Text.Printf (printf)

-- | Represent CPU Instruction of Hack Computer.
--
-- Read the complete [specification of the Hack instructions](https://www.nand2tetris.org/_files/ugd/44046b_89a8e226476741a3b7c5204575b8a0b2.pdf) for in-depth explanation.
data Instruction
  = -- | A instruction with 15 Bit value.
    A' Int
  | -- | C instruction consisting of 13 Bit command.
    C'
      { -- | 1 Bit value. When 0 computation is done with value of A Register, otherwise with memory value A register points to.
        _a :: Int,
        -- | 6 Bit computation instruction of the ALU
        _comp :: Int,
        -- | 3 Bit value which defines the destination where the result of the ALU computation is written to.
        _dest :: Int,
        -- | 3 Bit value which defines when to jump.
        _jump :: Int
      }
  deriving (Eq)

instance Show Instruction where
  show i = case i of
    A' n -> "A " <> showBin n ""
    C' a c d j -> "C " <> unwords (getZipList $ liftA2 showBin (ZipList [a, c, d, j]) $ ZipList $ repeat "")

type ParseErrorDescription = String

binary :: Instruction -> Text
binary i = case i of
  A' n -> "0 " <> T.pack (printf "%015b" n)
  C' a c d j -> "1 " <> T.pack (unwords (getZipList $ liftA2 showBin (ZipList [a, c, d, j]) $ ZipList $ repeat ""))

-- | Parse single Instruction and return 'Data.Either.Right' 'Instruction' if instruction could be parsed,
-- otherwise 'Data.Either.Left' 'ParseErrorDescription'.
parseInstruction :: Text -> Either ParseErrorDescription Instruction
parseInstruction instr
  | T.null instr = Left $ "Could not parse " <> unpack instr
  | T.length instr == 1 = parseC instr
  | otherwise =
    let h = T.head instr
        address = T.tail instr
     in case h of
          '@' -> parseA address
          _ -> parseC instr

-- | Parse single __A__ Instruction and return 'Data.Either.Right' 'Instruction' if @'address'@ is between 1 and 32767 (15 Bit),
-- otherwise 'Data.Either.Left' 'ParseErrorDescription'.
parseA :: Text -> Either ParseErrorDescription Instruction
parseA address = (decimal :: Reader Int) address >>= toInstr
  where
    toInstr (a, _)
      | a > 32767 = Left $ "Address " <> unpack address <> " is bigger than 15 bits."
      | otherwise = Right $ A' a

parseC :: Text -> Either ParseErrorDescription Instruction
parseC instr = maybe fallBack Right $ parse instr
  where
    parse = fromInstrSplit . splitCInstr
    fallBack = Left ("Could not parse computation command " <> unpack instr)

fromInstrSplit :: CInstrSplit -> Maybe Instruction
fromInstrSplit (CInstrSplit' dest comp jmp) = C' <$> a <*> comp' <*> dest' <*> jmp'
  where
    a
      | isCompElem computationAZeroLUT = Just 0
      | isCompElem computationAOneLUT = Just 1
      | otherwise = Nothing
    dest' = join (lookup <$> dest <*> pure destinationLUT) <|> Just 0
    comp' = a >>= lookup (fromMaybe "" comp) . getCompLUT
    jmp' = join (lookup <$> jmp <*> pure jumpLUT) <|> Just 0
    getCompLUT a' = if a' == 0 then computationAZeroLUT else computationAOneLUT
    isCompElem lut = fromMaybe False $ P.elem <$> comp <*> pure (fmap fst lut)

-- | Split-up text consisting of destination, computation and jump part.
data CInstrSplit = CInstrSplit'
  { _destS :: Maybe Text,
    _compS :: Maybe Text,
    _jumpS :: Maybe Text
  }
  deriving (Show, Eq)

splitCInstr :: Text -> CInstrSplit
splitCInstr instr =
  let dest = if hasDest then Just (L.head firstSplit) else Nothing
      compAndJump = if hasDest then firstSplit !! 1 else L.head firstSplit
   in splitCompAndJump (CInstrSplit' dest) compAndJump
  where
    firstSplit = T.split (== '=') instr
    hasDest = L.length firstSplit == 2

splitCompAndJump :: (Maybe Text -> Maybe Text -> CInstrSplit) -> Text -> CInstrSplit
splitCompAndJump f t = f comp jump
  where
    comp = if L.length secondSplit <= 2 then Just (L.head secondSplit) else Nothing
    jump = if L.length secondSplit == 2 then Just (secondSplit !! 1) else Nothing
    secondSplit = T.split (== ';') t

-- | Lookup table for __A=0__ ('_a') computation command which represents '_comp' of 'Instruction'.
computationAZeroLUT :: [(Text, Int)]
computationAZeroLUT =
  [ ("0", 42),
    ("1", 63),
    ("-1", 58),
    ("D", 12),
    ("A", 48),
    ("!D", 13),
    ("!A", 49),
    ("-D", 15),
    ("-A", 51),
    ("D+1", 31),
    ("A+1", 55),
    ("D-1", 14),
    ("A-1", 50),
    ("D+A", 2),
    ("D-A", 19),
    ("A-D", 7),
    ("D&A", 0),
    ("D|A", 21)
  ]

-- | Lookup table for __A=1__ ('_a') computation command which represents '_comp' of 'Instruction'.
computationAOneLUT :: [(Text, Int)]
computationAOneLUT =
  [ ("M", 48),
    ("!M", 49),
    ("-M", 51),
    ("M+1", 55),
    ("M-1", 50),
    ("D+M", 2),
    ("D-M", 19),
    ("M-D", 7),
    ("D&M", 0),
    ("D|M", 21)
  ]

-- | Lookup table for every destination which represents '_dest' of 'Instruction'.
destinationLUT :: [(Text, Int)]
destinationLUT =
  [ ("M", 1),
    ("D", 2),
    ("MD", 3),
    ("A", 4),
    ("AM", 5),
    ("AD", 6),
    ("AMD", 7)
  ]

jumpLUT :: [(Text, Int)]
jumpLUT =
  [ ("JGT", 1),
    ("JEQ", 2),
    ("JGE", 3),
    ("JLT", 4),
    ("JNE", 5),
    ("JLE", 6),
    ("JMP", 7)
  ]

-- | Remove whitespaces, empty lines and comments.
cleanUpCode :: Text -> Text
cleanUpCode =
  fromMaybe ""
    . stripSuffix "\n"
    . unlines
    . P.filter isCode
    . T.lines
    . T.filter (/= ' ')

-- | Return True if Text does NOT contain any white spaces, new lines or comments, otherwise False.
isCode :: Text -> Bool
isCode = and . fmap not . sequenceA [isComment, (== ""), (== "\n"), T.elem ' ', T.elem '\n']

-- | Return True if Text is comment, otherwise False.
isComment :: Text -> Bool
isComment = (== "//") . take 2 . strip
