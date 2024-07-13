module Hack.VmCompilerSpec (spec) where

import Hack.VmCompiler.Internal
  ( Segment (..),
    VmCommand (..),
    pCommand,
    pCommands,
    pSegment,
  )
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck.Instances.Text ()
import Text.Parsec (parse)

spec :: Spec
spec = do
  describe "pCommands" $ do
    it "parses multiple arithmetical commands" $ do
      parse pCommands "" "add\nsub\n" `shouldBe` Right [Add, Sub]
    it "parses multiple logical commands" $ do
      parse pCommands "" "eq\ngt\n" `shouldBe` Right [Eq, Gt]
    it "parses multiple push / pop commands" $ do
      parse pCommands "" "push constant 7\npop local 0\n"
        `shouldBe` Right [Push Constant 7, Pop Local 0]
    it "parses mixed commands" $ do
      parse pCommands "" "add\npush constant 7\n"
        `shouldBe` Right [Add, Push Constant 7]
      parse pCommands "" "eq\npop local 0\n" `shouldBe` Right [Eq, Pop Local 0]

  describe "pCommand" $ do
    it "parses arithmetical commands" $ do
      parse pCommand "" "add\n" `shouldBe` Right Add
      parse pCommand "" "sub\n" `shouldBe` Right Sub
      parse pCommand "" "neg\n" `shouldBe` Right Neg
    it "parses logical commands" $ do
      parse pCommand "" "eq\n" `shouldBe` Right Eq
      parse pCommand "" "gt\n" `shouldBe` Right Gt
      parse pCommand "" "lt\n" `shouldBe` Right Lt
      parse pCommand "" "and\n" `shouldBe` Right And
      parse pCommand "" "or\n" `shouldBe` Right Or
      parse pCommand "" "not\n" `shouldBe` Right Not
    it "parses push / pop commands" $ do
      parse pCommand "" "push constant 7\n" `shouldBe` Right (Push Constant 7)
      parse pCommand "" "pop local 0\n" `shouldBe` Right (Pop Local 0)

  describe "pSegment" $ do
    it "parses segment" $ do
      parse pSegment "" "constant " `shouldBe` Right Constant
      parse pSegment "" "local " `shouldBe` Right Local
      parse pSegment "" "argument " `shouldBe` Right Argument
      parse pSegment "" "this " `shouldBe` Right This
      parse pSegment "" "that " `shouldBe` Right That
      parse pSegment "" "temp " `shouldBe` Right Temp
      parse pSegment "" "pointer " `shouldBe` Right Pointer
      parse pSegment "" "static " `shouldBe` Right Static
