{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hack.AssemblerSpec (spec) where

import Data.Either
import Data.Text as T (Text, lines, pack, unlines)
import Hack.Assembler (compile, machineCode, parse)
import Hack.Assembler.Internal (CInstrSplit (..), Instruction (..), binary, buildLabelLUT, buildVariableLUT, cleanUpCode, convertLabels, convertPredefinedSymbols, convertSymbols, convertVariables, isCode, isComment, parseInstruction, splitCInstr)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck (elements, forAll, listOf1, property)
import Test.QuickCheck.Instances.Text ()
import Text.RawString.QQ (r)

spec :: Spec
spec = do
  describe "compile" $ do
    it "compiles assembly into machine code" $ do
      compile exampleAssemblyOne `shouldBe` Right exampleCompiledOne
      compile exampleAssemblyTwo `shouldBe` Right exampleCompiledTwo
      compile exampleAssemblyAdd `shouldBe` Right exampleCompiledAdd
      compile exampleAssemblyMax `shouldBe` Right exampleCompiledMax
      compile exampleAssemblyRect `shouldBe` Right exampleCompiledRect

  describe "Binary is member of Read class" $ do
    it "shows A Instruction" $ do
      show (A' 12) `shouldBe` "A 12"
      show (A' 100) `shouldBe` "A 100"
    it "shows C Instruction" $ do
      show (C' 1 8 6 7) `shouldBe` "C 1 8 6 7"
      show (C' 0 60 4 5) `shouldBe` "C 0 60 4 5"

  describe "parseInstruction of general invalid instructions" $ do
    it "handles empty command" $ do
      parseInstruction "" `shouldSatisfy` isLeft

  describe "parseInstruction of A instructions" $ do
    it "parses valid A instruction" $ do
      parseInstruction "@0" `shouldBe` Right (A' 0)
      parseInstruction "@1" `shouldBe` Right (A' 1)
      parseInstruction "@32767" `shouldBe` Right (A' 32767)
    it "handles address bigger than 15 bit" $ do
      parseInstruction "@32768" `shouldSatisfy` isLeft

  describe "machineCode" $ do
    it "translates various instructions" $ do
      machineCode exampleInstructionsTwo `shouldBe` machineCodeTwo

  describe "parse" $ do
    it "parses empty text" $ do
      parse "" `shouldBe` Right []
    it "mixed instructions" $ do
      parse exampleInstructionsTextOne `shouldBe` Right exampleInstructionsOne
      parse exampleInstructionsTextTwo `shouldBe` Right exampleInstructionsTwo
    it "handles errors" $ do
      parse "bla\nblub" `shouldSatisfy` isLeft

  describe "binary" $ do
    it "shows binary representation of instruction" $ do
      fmap binary exampleInstructionsTwo `shouldBe` binaryInstructionsTwo
      fmap binary exampleInstructionsThree `shouldBe` binaryInstructionsThree

  describe "parseInstruction of C instructions" $ do
    it "parses all possible computations for a=0" $ do
      parseInstruction "0" `shouldBe` Right (C' 0 42 0 0)
      parseInstruction "1" `shouldBe` Right (C' 0 63 0 0)
      parseInstruction "-1" `shouldBe` Right (C' 0 58 0 0)
      parseInstruction "D" `shouldBe` Right (C' 0 12 0 0)
      parseInstruction "A" `shouldBe` Right (C' 0 48 0 0)
      parseInstruction "!D" `shouldBe` Right (C' 0 13 0 0)
      parseInstruction "!A" `shouldBe` Right (C' 0 49 0 0)
      parseInstruction "-D" `shouldBe` Right (C' 0 15 0 0)
      parseInstruction "-A" `shouldBe` Right (C' 0 51 0 0)
      parseInstruction "D+1" `shouldBe` Right (C' 0 31 0 0)
      parseInstruction "A+1" `shouldBe` Right (C' 0 55 0 0)
      parseInstruction "D-1" `shouldBe` Right (C' 0 14 0 0)
      parseInstruction "A-1" `shouldBe` Right (C' 0 50 0 0)
      parseInstruction "D+A" `shouldBe` Right (C' 0 2 0 0)
      parseInstruction "D-A" `shouldBe` Right (C' 0 19 0 0)
      parseInstruction "A-D" `shouldBe` Right (C' 0 7 0 0)
      parseInstruction "D&A" `shouldBe` Right (C' 0 0 0 0)
      parseInstruction "D|A" `shouldBe` Right (C' 0 21 0 0)
    it "parses all possible computations for a=1" $ do
      parseInstruction "M" `shouldBe` Right (C' 1 48 0 0)
      parseInstruction "!M" `shouldBe` Right (C' 1 49 0 0)
      parseInstruction "-M" `shouldBe` Right (C' 1 51 0 0)
      parseInstruction "M+1" `shouldBe` Right (C' 1 55 0 0)
      parseInstruction "M-1" `shouldBe` Right (C' 1 50 0 0)
      parseInstruction "D+M" `shouldBe` Right (C' 1 2 0 0)
      parseInstruction "D-M" `shouldBe` Right (C' 1 19 0 0)
      parseInstruction "M-D" `shouldBe` Right (C' 1 7 0 0)
      parseInstruction "D&M" `shouldBe` Right (C' 1 0 0 0)
      parseInstruction "D|M" `shouldBe` Right (C' 1 21 0 0)
    it "parses all possible destinations for a=0" $ do
      parseInstruction "0" `shouldBe` Right (C' 0 42 0 0)
      parseInstruction "M=0" `shouldBe` Right (C' 0 42 1 0)
      parseInstruction "D=0" `shouldBe` Right (C' 0 42 2 0)
      parseInstruction "MD=0" `shouldBe` Right (C' 0 42 3 0)
      parseInstruction "A=0" `shouldBe` Right (C' 0 42 4 0)
      parseInstruction "AM=0" `shouldBe` Right (C' 0 42 5 0)
      parseInstruction "AD=0" `shouldBe` Right (C' 0 42 6 0)
      parseInstruction "AMD=0" `shouldBe` Right (C' 0 42 7 0)
    it "parses all possible destinations for a=1" $ do
      parseInstruction "M=M" `shouldBe` Right (C' 1 48 1 0)
      parseInstruction "D=!M" `shouldBe` Right (C' 1 49 2 0)
      parseInstruction "MD=-M" `shouldBe` Right (C' 1 51 3 0)
      parseInstruction "A=M+1" `shouldBe` Right (C' 1 55 4 0)
      parseInstruction "AM=M-1" `shouldBe` Right (C' 1 50 5 0)
      parseInstruction "AD=D+M" `shouldBe` Right (C' 1 2 6 0)
      parseInstruction "AMD=D-M" `shouldBe` Right (C' 1 19 7 0)
    it "parses all possible jumps for a=0" $ do
      parseInstruction "0;JGT" `shouldBe` Right (C' 0 42 0 1)
      parseInstruction "-1;JEQ" `shouldBe` Right (C' 0 58 0 2)
      parseInstruction "A;JGE" `shouldBe` Right (C' 0 48 0 3)
      parseInstruction "!D;JLT" `shouldBe` Right (C' 0 13 0 4)
      parseInstruction "-D;JNE" `shouldBe` Right (C' 0 15 0 5)
      parseInstruction "D+1;JLE" `shouldBe` Right (C' 0 31 0 6)
      parseInstruction "D|A;JMP" `shouldBe` Right (C' 0 21 0 7)
    it "parses all possible jumps with destination for a=0" $ do
      parseInstruction "M=0;JGT" `shouldBe` Right (C' 0 42 1 1)
      parseInstruction "D=-1;JEQ" `shouldBe` Right (C' 0 58 2 2)
      parseInstruction "MD=A;JGE" `shouldBe` Right (C' 0 48 3 3)
      parseInstruction "A=!D;JLT" `shouldBe` Right (C' 0 13 4 4)
      parseInstruction "AM=-D;JNE" `shouldBe` Right (C' 0 15 5 5)
      parseInstruction "AD=D+1;JLE" `shouldBe` Right (C' 0 31 6 6)
      parseInstruction "AMD=D|A;JMP" `shouldBe` Right (C' 0 21 7 7)

  describe "splitCInstr" $ do
    it "gets Computation part" $ do
      splitCInstr "0" `shouldBe` CInstrSplit' Nothing (Just "0") Nothing
      splitCInstr "1" `shouldBe` CInstrSplit' Nothing (Just "1") Nothing
      splitCInstr "-1" `shouldBe` CInstrSplit' Nothing (Just "-1") Nothing
      splitCInstr "D" `shouldBe` CInstrSplit' Nothing (Just "D") Nothing
      splitCInstr "A" `shouldBe` CInstrSplit' Nothing (Just "A") Nothing
      splitCInstr "!D" `shouldBe` CInstrSplit' Nothing (Just "!D") Nothing
      splitCInstr "!A" `shouldBe` CInstrSplit' Nothing (Just "!A") Nothing
      splitCInstr "-D" `shouldBe` CInstrSplit' Nothing (Just "-D") Nothing
      splitCInstr "-A" `shouldBe` CInstrSplit' Nothing (Just "-A") Nothing
      splitCInstr "D+1" `shouldBe` CInstrSplit' Nothing (Just "D+1") Nothing
      splitCInstr "A+1" `shouldBe` CInstrSplit' Nothing (Just "A+1") Nothing
      splitCInstr "D-1" `shouldBe` CInstrSplit' Nothing (Just "D-1") Nothing
      splitCInstr "A-1" `shouldBe` CInstrSplit' Nothing (Just "A-1") Nothing
      splitCInstr "D+A" `shouldBe` CInstrSplit' Nothing (Just "D+A") Nothing
      splitCInstr "D-A" `shouldBe` CInstrSplit' Nothing (Just "D-A") Nothing
      splitCInstr "A-D" `shouldBe` CInstrSplit' Nothing (Just "A-D") Nothing
      splitCInstr "D&A" `shouldBe` CInstrSplit' Nothing (Just "D&A") Nothing
      splitCInstr "D|A" `shouldBe` CInstrSplit' Nothing (Just "D|A") Nothing
      splitCInstr "M" `shouldBe` CInstrSplit' Nothing (Just "M") Nothing
      splitCInstr "!M" `shouldBe` CInstrSplit' Nothing (Just "!M") Nothing
      splitCInstr "-M" `shouldBe` CInstrSplit' Nothing (Just "-M") Nothing
      splitCInstr "M+1" `shouldBe` CInstrSplit' Nothing (Just "M+1") Nothing
      splitCInstr "M-1" `shouldBe` CInstrSplit' Nothing (Just "M-1") Nothing
      splitCInstr "D+M" `shouldBe` CInstrSplit' Nothing (Just "D+M") Nothing
      splitCInstr "D-M" `shouldBe` CInstrSplit' Nothing (Just "D-M") Nothing
      splitCInstr "M-D" `shouldBe` CInstrSplit' Nothing (Just "M-D") Nothing
      splitCInstr "D&M" `shouldBe` CInstrSplit' Nothing (Just "D&M") Nothing
      splitCInstr "D|M" `shouldBe` CInstrSplit' Nothing (Just "D|M") Nothing
    it "gets Destination and Computation part" $ do
      splitCInstr "M=0" `shouldBe` CInstrSplit' (Just "M") (Just "0") Nothing
      splitCInstr "D=1" `shouldBe` CInstrSplit' (Just "D") (Just "1") Nothing
      splitCInstr "MD=-1" `shouldBe` CInstrSplit' (Just "MD") (Just "-1") Nothing
      splitCInstr "A=D" `shouldBe` CInstrSplit' (Just "A") (Just "D") Nothing
      splitCInstr "AM=A" `shouldBe` CInstrSplit' (Just "AM") (Just "A") Nothing
      splitCInstr "AD=!D" `shouldBe` CInstrSplit' (Just "AD") (Just "!D") Nothing
      splitCInstr "AMD=D&A" `shouldBe` CInstrSplit' (Just "AMD") (Just "D&A") Nothing
      splitCInstr "AMD=D|A" `shouldBe` CInstrSplit' (Just "AMD") (Just "D|A") Nothing
    it "gets Computation and Jump part" $ do
      splitCInstr "0;JGT" `shouldBe` CInstrSplit' Nothing (Just "0") (Just "JGT")
      splitCInstr "1;JEQ" `shouldBe` CInstrSplit' Nothing (Just "1") (Just "JEQ")
      splitCInstr "-1;JGE" `shouldBe` CInstrSplit' Nothing (Just "-1") (Just "JGE")
      splitCInstr "D;JLT" `shouldBe` CInstrSplit' Nothing (Just "D") (Just "JLT")
      splitCInstr "!D;JNE" `shouldBe` CInstrSplit' Nothing (Just "!D") (Just "JNE")
      splitCInstr "D+1;JLE" `shouldBe` CInstrSplit' Nothing (Just "D+1") (Just "JLE")
      splitCInstr "D|M;JMP" `shouldBe` CInstrSplit' Nothing (Just "D|M") (Just "JMP")
    it "gets Destination, Computation and Jump part" $ do
      splitCInstr "M=0;JGT" `shouldBe` CInstrSplit' (Just "M") (Just "0") (Just "JGT")
      splitCInstr "D=1;JEQ" `shouldBe` CInstrSplit' (Just "D") (Just "1") (Just "JEQ")
      splitCInstr "MD=-1;JGE" `shouldBe` CInstrSplit' (Just "MD") (Just "-1") (Just "JGE")
      splitCInstr "A=D;JLT" `shouldBe` CInstrSplit' (Just "A") (Just "D") (Just "JLT")
      splitCInstr "AM=A;JNE" `shouldBe` CInstrSplit' (Just "AM") (Just "A") (Just "JNE")
      splitCInstr "AD=!D;JLE" `shouldBe` CInstrSplit' (Just "AD") (Just "!D") (Just "JLE")
      splitCInstr "AMD=D&A;JMP" `shouldBe` CInstrSplit' (Just "AMD") (Just "D&A") (Just "JMP")

  describe "convertSymbols" $ do
    it "converts virtual registers" $ do
      convertPredefinedSymbols (T.lines exampleVirtualRegisters) `shouldBe` T.lines exampleVirtualRegistersConverted
    it "converts predefined pointers" $ do
      convertPredefinedSymbols (T.lines examplePredefinedPointers) `shouldBe` T.lines examplePredefinedPointersConverted
    it "converts IO pointers" $ do
      convertPredefinedSymbols (T.lines exampleIOPointers) `shouldBe` T.lines exampleIOPointersConverted
    it "keeps korrekt addresses" $ do
      convertSymbols (T.lines exampleAlreadyCorrectAddresses) `shouldBe` T.lines exampleAlreadyCorrectAddresses

  describe "convertLabels" $ do
    it "converts labels to addresses" $ do
      convertLabels (T.lines exampleLabelText) `shouldBe` T.lines exampleLabelTextConverted

  describe "convertVariables" $ do
    it "converts variables to addresses" $ do
      convertVariables (T.lines exampleVariableText) `shouldBe` T.lines exampleVariableConverted

  describe "buildVariableLUT" $ do
    it "creates variable LUT" $ do
      buildVariableLUT (T.lines exampleVariableText) `shouldBe` exampleVariableLUT

  describe "buildLabelLUT" $ do
    it "creates label LUT and returns text with labels removed" $ do
      buildLabelLUT (T.lines exampleLabelText) `shouldBe` (T.lines exampleLabelTextRemoved, exampleLabelTextLUT)

  describe "cleanUpCode" $ do
    it "can handle empty text" $ do
      cleanUpCode "" `shouldBe` []
    it "can handle one line" $ do
      cleanUpCode "a" `shouldBe` ["a"]
    it "can handle two lines" $ do
      cleanUpCode "a\nb b" `shouldBe` ["a", "bb"]
      cleanUpCode "a \n b b" `shouldBe` ["a", "bb"]
    it "can handle multiple new lines" $ do
      cleanUpCode "a \n\n b\n \n b" `shouldBe` ["a", "b", "b"]
    it "can remove comments" $ do
      cleanUpCode "a\n// i am a comment" `shouldBe` ["a"]
      cleanUpCode "a\n// i am a comment\nabc" `shouldBe` ["a", "abc"]
      cleanUpCode "a \n // i am a comment \n abc " `shouldBe` ["a", "abc"]
    it "cleans up example code" $ do
      cleanUpCode exampleCleanUpBefore `shouldBe` exampleCleanUpAfter

  describe "isCode" $ do
    it "detects code" $
      property $
        let validElements = elements $ ['a' .. 'z'] <> ['0' .. '9']
            genValidText = pack <$> listOf1 validElements
         in forAll genValidText $ \t -> isCode t `shouldBe` True
    it "detects empty text" $ do
      isCode "" `shouldBe` False
    it "detects white spaces" $ do
      isCode " a" `shouldBe` False
      isCode "a " `shouldBe` False
      isCode "\n" `shouldBe` False

  describe "isComment" $ do
    it "can handle empty text" $ do
      isComment "" `shouldBe` False
    it "can handle non comments" $ do
      isComment "/" `shouldBe` False
      isComment " /" `shouldBe` False
      isComment " /   " `shouldBe` False
    it "can handle comments" $ do
      isComment "//" `shouldBe` True
      isComment "// jfal;kfja;lf" `shouldBe` True
      isComment "  // jfal;kfja;lf" `shouldBe` True

exampleCleanUpBefore :: Text
exampleCleanUpBefore =
  [r|// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/06/add/Add.asm

// Computes R0 = 2 + 3  (R0 refers to RAM[0])

@2
D=A
@3
D=D+A
@0
M=D
|]

exampleCleanUpAfter :: [Text]
exampleCleanUpAfter =
  [ "@2",
    "D=A",
    "@3",
    "D=D+A",
    "@0",
    "M=D"
  ]

exampleInstructionsTextOne :: Text
exampleInstructionsTextOne =
  [r|
0 
1 
-1 
D 
A 
!D 
!A 
-D 
-A 
D+1
A+1
D-1
A-1
D+A
D-A
A-D
D&A
D|A
  |]

exampleInstructionsOne :: [Instruction]
exampleInstructionsOne =
  [ C' 0 42 0 0,
    C' 0 63 0 0,
    C' 0 58 0 0,
    C' 0 12 0 0,
    C' 0 48 0 0,
    C' 0 13 0 0,
    C' 0 49 0 0,
    C' 0 15 0 0,
    C' 0 51 0 0,
    C' 0 31 0 0,
    C' 0 55 0 0,
    C' 0 14 0 0,
    C' 0 50 0 0,
    C' 0 2 0 0,
    C' 0 19 0 0,
    C' 0 7 0 0,
    C' 0 0 0 0,
    C' 0 21 0 0
  ]

exampleInstructionsTextTwo :: Text
exampleInstructionsTextTwo =
  [r|
M=0;JGT
D=-1;JEQ
MD=A;JGE
A=!D;JLT
AM=-D;JNE
AD=D+1;JLE
AMD=D|A;JMP
  |]

exampleInstructionsTwo :: [Instruction]
exampleInstructionsTwo =
  [ C' 0 42 1 1,
    C' 0 58 2 2,
    C' 0 48 3 3,
    C' 0 13 4 4,
    C' 0 15 5 5,
    C' 0 31 6 6,
    C' 0 21 7 7
  ]

binaryInstructionsTwo :: [Text]
binaryInstructionsTwo =
  [ "1 11 0 101010 001 001",
    "1 11 0 111010 010 010",
    "1 11 0 110000 011 011",
    "1 11 0 001101 100 100",
    "1 11 0 001111 101 101",
    "1 11 0 011111 110 110",
    "1 11 0 010101 111 111"
  ]

machineCodeTwo :: Text
machineCodeTwo =
  [r|1110101010001001
1110111010010010
1110110000011011
1110001101100100
1110001111101101
1110011111110110
1110010101111111
|]

exampleInstructionsThree :: [Instruction]
exampleInstructionsThree =
  [ A' 32767,
    A' 0,
    A' 111
  ]

binaryInstructionsThree :: [Text]
binaryInstructionsThree =
  [ "0 111111111111111",
    "0 000000000000000",
    "0 000000001101111"
  ]

exampleVirtualRegisters :: Text
exampleVirtualRegisters =
  [r|@R0
@R1
@R2
@R3
@R4
@R5
@R6
@R7
@R8
@R9
@R10
@R11
@R12
@R13
@R14
@R15
|]

exampleVirtualRegistersConverted :: Text
exampleVirtualRegistersConverted = genAddressInstr 15

examplePredefinedPointers :: Text
examplePredefinedPointers =
  [r|@SP
@LCL
@ARG
@THIS
@THAT
|]

examplePredefinedPointersConverted :: Text
examplePredefinedPointersConverted = genAddressInstr 4

exampleIOPointers :: Text
exampleIOPointers =
  [r|@SCREEN
@KBD|]

exampleIOPointersConverted :: Text
exampleIOPointersConverted =
  [r|@16384
@24576
|]

exampleAlreadyCorrectAddresses :: Text
exampleAlreadyCorrectAddresses =
  [r|@2
D=A
@3
D=D+A
@0
M=D|]

exampleLabelText :: Text
exampleLabelText =
  [r|@i
M=0
@R2
M=0
(LOOP)
@R1
D=M
@i
D=D-M
@END
D;JEQ
@R0
D=M
@R2
M=D+M
@i
M=M+1
@LOOP
D;JMP
(END)
@END
0;JMP|]

exampleLabelTextRemoved :: Text
exampleLabelTextRemoved =
  [r|@i
M=0
@R2
M=0
@R1
D=M
@i
D=D-M
@END
D;JEQ
@R0
D=M
@R2
M=D+M
@i
M=M+1
@LOOP
D;JMP
@END
0;JMP|]

exampleLabelTextConverted :: Text
exampleLabelTextConverted =
  [r|@i
M=0
@R2
M=0
@R1
D=M
@i
D=D-M
@18
D;JEQ
@R0
D=M
@R2
M=D+M
@i
M=M+1
@4
D;JMP
@18
0;JMP|]

exampleLabelTextLUT :: [(Text, Int)]
exampleLabelTextLUT = [("END", 18), ("LOOP", 4)]

exampleVariableText :: Text
exampleVariableText =
  [r|@i
M=0
@R2
@var
M=0
@k
(LOOP)
@R1
D=M
@i
D=D-M
@END
@k
D;JEQ
@R0
D=M
@R2
M=D+M
@i
M=M+1
@LOOP
@var
D;JMP
(END)
@END
0;JMP|]

exampleVariableConverted :: Text
exampleVariableConverted =
  [r|@16
M=0
@R2
@17
M=0
@18
(LOOP)
@R1
D=M
@16
D=D-M
@END
@18
D;JEQ
@R0
D=M
@R2
M=D+M
@16
M=M+1
@LOOP
@17
D;JMP
(END)
@END
0;JMP|]

exampleVariableLUT :: [(Text, Int)]
exampleVariableLUT = [("k", 18), ("var", 17), ("i", 16)]

genAddressInstr :: Int -> Text
genAddressInstr n = T.unlines ["@" <> T.pack (show x) | x <- [0 .. n :: Int]]

exampleAssemblyOne :: Text
exampleAssemblyOne =
  [r|// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)
//
// This program only needs to handle arguments that satisfy
// R0 >= 0, R1 >= 0, and R0*R1 < 32768.

  // initialize i
  @i
  M=0

  // initialize @R2
  @R2
  M=0

(LOOP)
  // if (i == R1) goto END
  @R1
  D=M
  @i
  D=D-M

  @END
  D;JEQ

  @R0
  D=M
  @R2
  M=D+M

  @i
  M=M+1

  @LOOP
  D;JMP

(END)
  @END
  0;JMP|]

exampleCompiledOne :: Text
exampleCompiledOne =
  [r|0000000000010000
1110101010001000
0000000000000010
1110101010001000
0000000000000001
1111110000010000
0000000000010000
1111010011010000
0000000000010010
1110001100000010
0000000000000000
1111110000010000
0000000000000010
1111000010001000
0000000000010000
1111110111001000
0000000000000100
1110001100000111
0000000000010010
1110101010000111
|]

exampleAssemblyTwo :: Text
exampleAssemblyTwo =
  [r|// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed.
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// Put your code here.
(WAITFORINPUT)
  @KBD
  D=M
  @CHECKWASBLACKBEFORE
  D;JNE

  @CHECKWASWHITEBEFORE
  0;JMP

(CHECKWASWHITEBEFORE)
  @color
  D=M
  @WHITE
  D;JNE
  @WAITFORINPUT
  0;JMP

  @WAITFORINPUT
  D;JMP

(CHECKWASBLACKBEFORE)
  @color
  D=M
  @BLACK
  D;JEQ
  @WAITFORINPUT
  0;JMP

(WHITE)
  @color
  M=0
  @SETSCREEN
  0;JMP

(BLACK)
  @color
  M=-1

(SETSCREEN)
  @SCREEN
  D=A
  @currentscreen
  M=D

  (SETSCREENPART)
    @KBD
    D=A
    @currentscreen
    D=D-M
    @WAITFORINPUT
    D;JEQ
    @color
    D=M
    @currentscreen
    A=M
    M=D
    @currentscreen
    M=M+1
    @SETSCREENPART
    0;JMP|]

exampleCompiledTwo :: Text
exampleCompiledTwo =
  [r|0110000000000000
1111110000010000
0000000000001110
1110001100000101
0000000000000110
1110101010000111
0000000000010000
1111110000010000
0000000000010100
1110001100000101
0000000000000000
1110101010000111
0000000000000000
1110001100000111
0000000000010000
1111110000010000
0000000000011000
1110001100000010
0000000000000000
1110101010000111
0000000000010000
1110101010001000
0000000000011010
1110101010000111
0000000000010000
1110111010001000
0100000000000000
1110110000010000
0000000000010001
1110001100001000
0110000000000000
1110110000010000
0000000000010001
1111010011010000
0000000000000000
1110001100000010
0000000000010000
1111110000010000
0000000000010001
1111110000100000
1110001100001000
0000000000010001
1111110111001000
0000000000011110
1110101010000111
|]

exampleAssemblyAdd :: Text
exampleAssemblyAdd =
  [r|// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/06/add/Add.asm

// Computes R0 = 2 + 3  (R0 refers to RAM[0])

@2
D=A
@3
D=D+A
@0
M=D|]

exampleCompiledAdd :: Text
exampleCompiledAdd =
  [r|0000000000000010
1110110000010000
0000000000000011
1110000010010000
0000000000000000
1110001100001000
|]

exampleAssemblyMax :: Text
exampleAssemblyMax =
  [r|// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/06/max/Max.asm

// Computes R2 = max(R0, R1)  (R0,R1,R2 refer to RAM[0],RAM[1],RAM[2])

   @R0
   D=M              // D = first number
   @R1
   D=D-M            // D = first number - second number
   @OUTPUT_FIRST
   D;JGT            // if D>0 (first is greater) goto output_first
   @R1
   D=M              // D = second number
   @OUTPUT_D
   0;JMP            // goto output_d
(OUTPUT_FIRST)
   @R0
   D=M              // D = first number
(OUTPUT_D)
   @R2
   M=D              // M[2] = D (greatest number)
(INFINITE_LOOP)
   @INFINITE_LOOP
   0;JMP            // infinite loop
|]

exampleCompiledMax :: Text
exampleCompiledMax =
  [r|0000000000000000
1111110000010000
0000000000000001
1111010011010000
0000000000001010
1110001100000001
0000000000000001
1111110000010000
0000000000001100
1110101010000111
0000000000000000
1111110000010000
0000000000000010
1110001100001000
0000000000001110
1110101010000111
|]

exampleAssemblyRect :: Text
exampleAssemblyRect =
  [r|// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/06/rect/Rect.asm

// Draws a rectangle at the top-left corner of the screen.
// The rectangle is 16 pixels wide and R0 pixels high.

   @0
   D=M
   @INFINITE_LOOP
   D;JLE
   @counter
   M=D
   @SCREEN
   D=A
   @address
   M=D
(LOOP)
   @address
   A=M
   M=-1
   @address
   D=M
   @32
   D=D+A
   @address
   M=D
   @counter
   MD=M-1
   @LOOP
   D;JGT
(INFINITE_LOOP)
   @INFINITE_LOOP
   0;JMP
|]
 
exampleCompiledRect :: Text
exampleCompiledRect =
  [r|0000000000000000
1111110000010000
0000000000010111
1110001100000110
0000000000010000
1110001100001000
0100000000000000
1110110000010000
0000000000010001
1110001100001000
0000000000010001
1111110000100000
1110111010001000
0000000000010001
1111110000010000
0000000000100000
1110000010010000
0000000000010001
1110001100001000
0000000000010000
1111110010011000
0000000000001010
1110001100000001
0000000000010111
1110101010000111
|]
 