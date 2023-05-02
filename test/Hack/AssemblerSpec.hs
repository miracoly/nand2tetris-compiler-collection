{-# LANGUAGE OverloadedStrings #-}

module Hack.AssemblerSpec (spec) where

import Data.Either
import Data.Text (pack)
import Hack.Assembler.Internal (Instruction (..), cleanUpCode, isCode, isComment, parseInstruction, Instruction(..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck (elements, forAll, listOf1, property)
import Test.QuickCheck.Instances.Text ()

spec :: Spec
spec = do
  describe "Binary is member of Read class" $ do
    it "shows A Instruction" $ do
      show (A' 12) `shouldBe` "A 1100"
      show (A' 100) `shouldBe` "A 1100100"
    it "shows C Instruction" $ do
      show (C' 1 8 6 7) `shouldBe` "C 1 1000 110 111"
      show (C' 0 60 4 5) `shouldBe` "C 0 111100 100 101"

  describe "parseInstruction of A instructions" $ do
    it "handles empty command" $ do
      parseInstruction "" `shouldSatisfy` isLeft
    it "handles address of 0" $ do
      parseInstruction "@0" `shouldSatisfy` isLeft
    it "parses valid A instruction" $ do
      parseInstruction "@1" `shouldBe` Right (A' 1)
      parseInstruction "@32767" `shouldBe` Right (A' 32767)
    it "handles address bigger than 15 bit" $ do
      parseInstruction "@32768" `shouldSatisfy` isLeft

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
