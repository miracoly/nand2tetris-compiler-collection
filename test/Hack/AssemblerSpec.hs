{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hack.AssemblerSpec (spec) where

import Data.Either
import Data.Text (Text, pack)
import Hack.Assembler (parse)
import Hack.Assembler.Internal (CInstrSplit (..), Instruction (..), cleanUpCode, isCode, isComment, parseInstruction, splitCInstr, binary)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck (elements, forAll, listOf1, property)
import Test.QuickCheck.Instances.Text ()
import Text.RawString.QQ (r)

spec :: Spec
spec = do
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

  describe "cleanUpCode" $ do
    it "can handle empty text" $ do
      cleanUpCode "" `shouldBe` ""
    it "can handle one line" $ do
      cleanUpCode "a" `shouldBe` "a"
    it "can handle two lines" $ do
      cleanUpCode "a\nb b" `shouldBe` "a\nbb"
      cleanUpCode "a \n b b" `shouldBe` "a\nbb"
    it "can handle multiple new lines" $ do
      cleanUpCode "a \n\n b\n \n b" `shouldBe` "a\nb\nb"
    it "can remove comments" $ do
      cleanUpCode "a\n// i am a comment" `shouldBe` "a"
      cleanUpCode "a\n// i am a comment\nabc" `shouldBe` "a\nabc"
      cleanUpCode "a \n // i am a comment \n abc " `shouldBe` "a\nabc"

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
  [ "1 0 101010 001 001",
    "1 0 111010 010 010",
    "1 0 110000 011 011",
    "1 0 001101 100 100",
    "1 0 001111 101 101",
    "1 0 011111 110 110",
    "1 0 010101 111 111"
  ]
  
exampleInstructionsThree :: [Instruction]
exampleInstructionsThree =
  [
    A' 32767,
    A' 0,
    A' 111
  ]

binaryInstructionsThree :: [Text]
binaryInstructionsThree = 
  [ "0 111111111111111",
    "0 000000000000000",
    "0 000000001101111"
  ]