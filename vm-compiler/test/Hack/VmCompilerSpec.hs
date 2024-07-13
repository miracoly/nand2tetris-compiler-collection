module Hack.VmCompilerSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck.Instances.Text ()
import Hack.VmCompiler (pLines)
import Text.Parsec (parse)

spec :: Spec
spec = do
  describe "pLines" $ do
    it "parses one line" $ do
      parse pLines "" "one line\n" `shouldBe` Right ["one line"]
    it "parses multiple lines" $ do
      let text = "line one\nline two\n"
      let expected = ["line one", "line two"]
      parse pLines "" text `shouldBe` Right expected
