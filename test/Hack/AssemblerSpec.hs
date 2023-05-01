{-# LANGUAGE OverloadedStrings #-}

module Hack.AssemblerSpec (spec) where

import Data.Text (pack)
import Hack.Assembler.Internal (cleanUpCode, isCode, isComment, Instruction(..))
import Test.Hspec
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
