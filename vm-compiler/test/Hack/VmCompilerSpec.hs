module Hack.VmCompilerSpec (spec) where

import Hack.VmCompiler.Internal (pLines, pIndex, pVmSegment, Segment(..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck.Instances.Text ()
import Text.Parsec (parse)

spec :: Spec
spec = do
  describe "pVmSegment" $ do
    it "parses segment" $ do
      parse pVmSegment "" "constant " `shouldBe` Right Constant
      parse pVmSegment "" "local " `shouldBe` Right Local
      parse pVmSegment "" "argument " `shouldBe` Right Argument
      parse pVmSegment "" "this " `shouldBe` Right This
      parse pVmSegment "" "that " `shouldBe` Right That
      parse pVmSegment "" "temp " `shouldBe` Right Temp
      parse pVmSegment "" "pointer " `shouldBe` Right Pointer
      parse pVmSegment "" "static " `shouldBe` Right Static

  describe "pIndex" $ do
    it "parses index" $ do
      parse pIndex "" "1\n" `shouldBe` Right 1
      parse pIndex "" "2\n" `shouldBe` Right 2
      parse pIndex "" "999\n" `shouldBe` Right 999

  describe "pLines" $ do
    it "parses one line" $ do
      parse pLines "" "one line\n" `shouldBe` Right ["one line"]
    it "parses multiple lines" $ do
      let text = "line one\nline two\n"
      let expected = ["line one", "line two"]
      parse pLines "" text `shouldBe` Right expected
