{-# LANGUAGE OverloadedStrings #-}

module Hack.AssemblerSpec (spec) where

import Hack.Assembler.Internal (cleanUpCode, isComment)
import Test.Hspec
import Test.QuickCheck.Instances.Text ()

spec :: Spec
spec = do
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
