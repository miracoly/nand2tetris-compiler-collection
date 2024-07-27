{-# LANGUAGE QuasiQuotes #-}

module Hack.VmCompilerSpec (spec) where

import Control.Monad.Reader (runReader)
import Hack.VmCompiler (compile)
import Hack.VmCompiler.Internal
  ( VmCommand (..),
    VmLine (..),
    VmSegment (..),
    pCommand,
    pComment,
    pLines,
    pSegment,
    parseVmLines,
    translateAdd,
    translateAnd,
    translateEq,
    translateGt,
    translateLt,
    translateNeg,
    translateNot,
    translateOr,
    translatePush,
    translateSeg,
    translateSub,
  )
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Parsec (parse)
import Text.RawString.QQ (r)

spec :: Spec
spec = do
  describe "compile" $ do
    it "compiles BasicTest.asm" $ do
      runReader (compile basicTestVm) "" `shouldBe` Right basicTestAsm
    it "compiles PointerTest.asm" $ do
      runReader (compile pointerTestVm) "" `shouldBe` Right pointerTestAsm
    it "compiles SimpleAdd.asm" $ do
      runReader (compile simpleAddVm) "" `shouldBe` Right simpleAddAsm
    it "compiles StackTest.asm" $ do
      runReader (compile stackTest) "" `shouldBe` Right stackTestAsm
    it "compiles StaticTest.asm" $ do
      runReader (compile staticTest) "" `shouldBe` Right staticTestAsm

  describe "translating" $ do
    describe "translate arithmetical commands" $ do
      it "translates add command" $ do
        translateAdd
          `shouldBe` [ "@SP",
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
      it "translates sub command" $ do
        translateSub
          `shouldBe` [ "@SP",
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
      it "translates neg command" $ do
        translateNeg
          `shouldBe` [ "@SP",
                       "M=M-1",
                       "A=M",
                       "M=-M",
                       "@SP",
                       "M=M+1"
                     ]
      it "translates eq command" $ do
        translateEq 9
          `shouldBe` [ "@SP",
                       "AM=M-1",
                       "D=M",
                       "@SP",
                       "AM=M-1",
                       "D=M-D",
                       "@EQUAL_9",
                       "D;JEQ",
                       "@SP",
                       "A=M",
                       "M=0",
                       "@END_EQ_9",
                       "0;JMP",
                       "(EQUAL_9)",
                       "@SP",
                       "A=M",
                       "M=-1",
                       "(END_EQ_9)",
                       "@SP",
                       "M=M+1"
                     ]
      it "translates gt command" $ do
        translateGt 9
          `shouldBe` [ "@SP",
                       "AM=M-1",
                       "D=M",
                       "@SP",
                       "AM=M-1",
                       "D=M-D",
                       "@GREATER_9",
                       "D;JGT",
                       "@SP",
                       "A=M",
                       "M=0",
                       "@END_GT_9",
                       "0;JMP",
                       "(GREATER_9)",
                       "@SP",
                       "A=M",
                       "M=-1",
                       "(END_GT_9)",
                       "@SP",
                       "M=M+1"
                     ]
      it "translates lt command" $ do
        translateLt 9
          `shouldBe` [ "@SP",
                       "AM=M-1",
                       "D=M",
                       "@SP",
                       "AM=M-1",
                       "D=M-D",
                       "@LESS_9",
                       "D;JLT",
                       "@SP",
                       "A=M",
                       "M=0",
                       "@END_LT_9",
                       "0;JMP",
                       "(LESS_9)",
                       "@SP",
                       "A=M",
                       "M=-1",
                       "(END_LT_9)",
                       "@SP",
                       "M=M+1"
                     ]
      it "translates and command" $ do
        translateAnd
          `shouldBe` [ "@SP",
                       "AM=M-1",
                       "D=M",
                       "@SP",
                       "AM=M-1",
                       "M=D&M",
                       "@SP",
                       "M=M+1"
                     ]
      it "translates or command" $ do
        translateOr
          `shouldBe` [ "@SP",
                       "AM=M-1",
                       "D=M",
                       "@SP",
                       "AM=M-1",
                       "M=D|M",
                       "@SP",
                       "M=M+1"
                     ]
      it "translates not command" $ do
        translateNot
          `shouldBe` [ "@SP",
                       "AM=M-1",
                       "M=!M",
                       "@SP",
                       "M=M+1"
                     ]

    describe "translatePush" $ do
      it "translates push local commands" $ do
        runReader (translatePush Local 0) ""
          `shouldBe` [ "@0",
                       "D=A",
                       "@LCL",
                       "A=D+M",
                       "D=M",
                       "@SP",
                       "A=M",
                       "M=D",
                       "@SP",
                       "M=M+1"
                     ]
        runReader (translatePush Local 1) ""
          `shouldBe` [ "@1",
                       "D=A",
                       "@LCL",
                       "A=D+M",
                       "D=M",
                       "@SP",
                       "A=M",
                       "M=D",
                       "@SP",
                       "M=M+1"
                     ]
      it "translates push constant commands" $ do
        runReader (translatePush Constant 7) ""
          `shouldBe` [ "@7",
                       "D=A",
                       "@SP",
                       "A=M",
                       "M=D",
                       "@SP",
                       "M=M+1"
                     ]
        runReader (translatePush Constant 17) ""
          `shouldBe` [ "@17",
                       "D=A",
                       "@SP",
                       "A=M",
                       "M=D",
                       "@SP",
                       "M=M+1"
                     ]

      it "translates push argument commands" $ do
        runReader (translatePush Argument 0) ""
          `shouldBe` [ "@0",
                       "D=A",
                       "@ARG",
                       "A=D+M",
                       "D=M",
                       "@SP",
                       "A=M",
                       "M=D",
                       "@SP",
                       "M=M+1"
                     ]
        runReader (translatePush Argument 1) ""
          `shouldBe` [ "@1",
                       "D=A",
                       "@ARG",
                       "A=D+M",
                       "D=M",
                       "@SP",
                       "A=M",
                       "M=D",
                       "@SP",
                       "M=M+1"
                     ]

      it "translates push this commands" $ do
        runReader (translatePush This 0) ""
          `shouldBe` [ "@0",
                       "D=A",
                       "@THIS",
                       "A=D+M",
                       "D=M",
                       "@SP",
                       "A=M",
                       "M=D",
                       "@SP",
                       "M=M+1"
                     ]
        runReader (translatePush This 1) ""
          `shouldBe` [ "@1",
                       "D=A",
                       "@THIS",
                       "A=D+M",
                       "D=M",
                       "@SP",
                       "A=M",
                       "M=D",
                       "@SP",
                       "M=M+1"
                     ]

      it "translates push that commands" $ do
        runReader (translatePush That 0) ""
          `shouldBe` [ "@0",
                       "D=A",
                       "@THAT",
                       "A=D+M",
                       "D=M",
                       "@SP",
                       "A=M",
                       "M=D",
                       "@SP",
                       "M=M+1"
                     ]
        runReader (translatePush That 1) ""
          `shouldBe` [ "@1",
                       "D=A",
                       "@THAT",
                       "A=D+M",
                       "D=M",
                       "@SP",
                       "A=M",
                       "M=D",
                       "@SP",
                       "M=M+1"
                     ]

      it "translates push temp commands" $ do
        runReader (translatePush Temp 1) ""
          `shouldBe` [ "@1",
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
        runReader (translatePush Temp 7) ""
          `shouldBe` [ "@7",
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

      it "translates push pointer commands" $ do
        runReader (translatePush Pointer 0) ""
          `shouldBe` [ "@THIS",
                       "D=M",
                       "@SP",
                       "A=M",
                       "M=D",
                       "@SP",
                       "M=M+1"
                     ]
        runReader (translatePush Pointer 1) ""
          `shouldBe` [ "@THAT",
                       "D=M",
                       "@SP",
                       "A=M",
                       "M=D",
                       "@SP",
                       "M=M+1"
                     ]

      it "translates push static commands" $ do
        runReader (translatePush Static 9) "TestFile.vm"
          `shouldBe` [ "@TestFile.vm.9",
                       "D=M",
                       "@SP",
                       "A=M",
                       "M=D",
                       "@SP",
                       "M=M+1"
                     ]

    describe "translateSeg" $ do
      it "translates segments" $ do
        translateSeg Constant 7 `shouldBe` "SP"
        translateSeg Local 0 `shouldBe` "LCL"
        translateSeg Argument 1 `shouldBe` "ARG"
        translateSeg This 2 `shouldBe` "THIS"
        translateSeg That 3 `shouldBe` "THAT"
        translateSeg Temp 4 `shouldBe` "R9"
        translateSeg Temp 0 `shouldBe` "R5"
        translateSeg Temp 7 `shouldBe` "R12"
        translateSeg Pointer 0 `shouldBe` "THIS"
        translateSeg Pointer 1 `shouldBe` "THAT"

  describe "parsing" $ do
    describe "parseVmLines" $ do
      it "parses multiple arithmetical commands" $ do
        parseVmLines "add\nsub\n" `shouldBe` Right [Command Add, Command Sub]
      it "parses multiple logical commands" $ do
        parseVmLines "eq\ngt\n" `shouldBe` Right [Command Eq, Command Gt]
      it "parses multiple push / pop commands" $ do
        parseVmLines "push constant 7\npop local 0\n"
          `shouldBe` Right [Command $ Push Constant 7, Command $ Pop Local 0]
      it "parses comments" $ do
        parseVmLines "// comment\n" `shouldBe` Right [Comment "comment"]
      it "parses mixed commands" $ do
        parseVmLines "add\npush constant 7\n"
          `shouldBe` Right [Command Add, Command $ Push Constant 7]
        parseVmLines "eq\npop local 0\n"
          `shouldBe` Right [Command Eq, Command $ Pop Local 0]
      it "parses mixed commands with comments" $ do
        parseVmLines "add\n// comment\n"
          `shouldBe` Right [Command Add, Comment "comment"]
        parseVmLines "pop this 1\n// comment\n"
          `shouldBe` Right [Command $ Pop This 1, Comment "comment"]

    describe "pLines" $ do
      it "parses multiple arithmetical commands" $ do
        parse pLines "" "add\nsub\n" `shouldBe` Right [Command Add, Command Sub]
      it "parses multiple logical commands" $ do
        parse pLines "" "eq\ngt\n" `shouldBe` Right [Command Eq, Command Gt]
      it "parses multiple push / pop commands" $ do
        parse pLines "" "push constant 7\npop local 0\n"
          `shouldBe` Right [Command $ Push Constant 7, Command $ Pop Local 0]
      it "parses comments" $ do
        parse pLines "" "// comment\n" `shouldBe` Right [Comment "comment"]
      it "parses mixed commands" $ do
        parse pLines "" "add\npush constant 7\n"
          `shouldBe` Right [Command Add, Command $ Push Constant 7]
        parse pLines "" "eq\npop local 0\n"
          `shouldBe` Right [Command Eq, Command $ Pop Local 0]
      it "parses mixed commands with comments" $ do
        parse pLines "" "add\n// comment\n"
          `shouldBe` Right [Command Add, Comment "comment"]
        parse pLines "" "pop this 1\n// comment\n"
          `shouldBe` Right [Command $ Pop This 1, Comment "comment"]
      it "parses mixed commands with multiple blank lines" $ do
        parse pLines "" "add\n\n\npush constant 7\n\n"
          `shouldBe` Right [Command Add, Command $ Push Constant 7]

    describe "pComment" $ do
      it "parses comments" $ do
        parse pComment "" "//comment\n" `shouldBe` Right "comment"
      it "parses comments and trims whitespaces" $ do
        parse pComment "" "//  comment  \n" `shouldBe` Right "comment"

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

basicTestVm :: String
basicTestVm =
  [r|// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/7/MemoryAccess/BasicTest/BasicTest.vm

// Executes pop and push commands.

push constant 10
pop local 0
push constant 21
push constant 22
pop argument 2
pop argument 1
push constant 36
pop this 6
push constant 42
push constant 45
pop that 5
pop that 2
push constant 510
pop temp 6
push local 0
push that 5
add
push argument 1
sub
push this 6
push this 6
add
sub
push temp 6
add
|]

basicTestAsm :: String
basicTestAsm =
  [r|// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/7/MemoryAccess/BasicTest/BasicTest.vm
// Executes pop and push commands.
@10
D=A
@SP
A=M
M=D
@SP
M=M+1
@0
D=A
@LCL
D=D+M
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
@21
D=A
@SP
A=M
M=D
@SP
M=M+1
@22
D=A
@SP
A=M
M=D
@SP
M=M+1
@2
D=A
@ARG
D=D+M
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
@1
D=A
@ARG
D=D+M
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
@36
D=A
@SP
A=M
M=D
@SP
M=M+1
@6
D=A
@THIS
D=D+M
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
@42
D=A
@SP
A=M
M=D
@SP
M=M+1
@45
D=A
@SP
A=M
M=D
@SP
M=M+1
@5
D=A
@THAT
D=D+M
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
@2
D=A
@THAT
D=D+M
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
@510
D=A
@SP
A=M
M=D
@SP
M=M+1
@6
D=A
@5
D=D+A
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
@0
D=A
@LCL
A=D+M
D=M
@SP
A=M
M=D
@SP
M=M+1
@5
D=A
@THAT
A=D+M
D=M
@SP
A=M
M=D
@SP
M=M+1
@SP
M=M-1
A=M
D=M
@SP
M=M-1
A=M
M=D+M
@SP
M=M+1
@1
D=A
@ARG
A=D+M
D=M
@SP
A=M
M=D
@SP
M=M+1
@SP
M=M-1
A=M
D=M
@SP
M=M-1
A=M
M=M-D
@SP
M=M+1
@6
D=A
@THIS
A=D+M
D=M
@SP
A=M
M=D
@SP
M=M+1
@6
D=A
@THIS
A=D+M
D=M
@SP
A=M
M=D
@SP
M=M+1
@SP
M=M-1
A=M
D=M
@SP
M=M-1
A=M
M=D+M
@SP
M=M+1
@SP
M=M-1
A=M
D=M
@SP
M=M-1
A=M
M=M-D
@SP
M=M+1
@6
D=A
@5
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
@SP
M=M-1
A=M
D=M
@SP
M=M-1
A=M
M=D+M
@SP
M=M+1
|]

pointerTestVm :: String
pointerTestVm =
  [r|// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/7/MemoryAccess/PointerTest/PointerTest.vm

// Executes pop and push commands using the 
// pointer, this, and that segments.

push constant 3030
pop pointer 0
push constant 3040
pop pointer 1
push constant 32
pop this 2
push constant 46
pop that 6
push pointer 0
push pointer 1
add
push this 2
sub
push that 6
add
|]

pointerTestAsm :: String
pointerTestAsm =
  [r|// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/7/MemoryAccess/PointerTest/PointerTest.vm
// Executes pop and push commands using the
// pointer, this, and that segments.
@3030
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
M=M-1
A=M
D=M
@THIS
M=D
@3040
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
M=M-1
A=M
D=M
@THAT
M=D
@32
D=A
@SP
A=M
M=D
@SP
M=M+1
@2
D=A
@THIS
D=D+M
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
@46
D=A
@SP
A=M
M=D
@SP
M=M+1
@6
D=A
@THAT
D=D+M
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1
@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1
@SP
M=M-1
A=M
D=M
@SP
M=M-1
A=M
M=D+M
@SP
M=M+1
@2
D=A
@THIS
A=D+M
D=M
@SP
A=M
M=D
@SP
M=M+1
@SP
M=M-1
A=M
D=M
@SP
M=M-1
A=M
M=M-D
@SP
M=M+1
@6
D=A
@THAT
A=D+M
D=M
@SP
A=M
M=D
@SP
M=M+1
@SP
M=M-1
A=M
D=M
@SP
M=M-1
A=M
M=D+M
@SP
M=M+1
|]

simpleAddVm :: String
simpleAddVm =
  [r|// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/7/StackArithmetic/SimpleAdd/SimpleAdd.vm

// Pushes and adds two constants.

push constant 7
push constant 8
add
|]

simpleAddAsm :: String
simpleAddAsm =
  [r|// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/7/StackArithmetic/SimpleAdd/SimpleAdd.vm
// Pushes and adds two constants.
@7
D=A
@SP
A=M
M=D
@SP
M=M+1
@8
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
M=M-1
A=M
D=M
@SP
M=M-1
A=M
M=D+M
@SP
M=M+1
|]

stackTest :: String
stackTest =
  [r|// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/7/StackArithmetic/StackTest/StackTest.vm

// Executes a sequence of arithmetic and logical operations on the stack. 

push constant 17
push constant 17
eq
push constant 17
push constant 16
eq
push constant 16
push constant 17
eq
push constant 892
push constant 891
lt
push constant 891
push constant 892
lt
push constant 891
push constant 891
lt
push constant 32767
push constant 32766
gt
push constant 32766
push constant 32767
gt
push constant 32766
push constant 32766
gt
push constant 57
push constant 31
push constant 53
add
push constant 112
sub
neg
and
push constant 82
or
not
|]

stackTestAsm :: String
stackTestAsm =
  [r|// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/7/StackArithmetic/StackTest/StackTest.vm
// Executes a sequence of arithmetic and logical operations on the stack.
@17
D=A
@SP
A=M
M=D
@SP
M=M+1
@17
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
@SP
AM=M-1
D=M-D
@EQUAL_0
D;JEQ
@SP
A=M
M=0
@END_EQ_0
0;JMP
(EQUAL_0)
@SP
A=M
M=-1
(END_EQ_0)
@SP
M=M+1
@17
D=A
@SP
A=M
M=D
@SP
M=M+1
@16
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
@SP
AM=M-1
D=M-D
@EQUAL_1
D;JEQ
@SP
A=M
M=0
@END_EQ_1
0;JMP
(EQUAL_1)
@SP
A=M
M=-1
(END_EQ_1)
@SP
M=M+1
@16
D=A
@SP
A=M
M=D
@SP
M=M+1
@17
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
@SP
AM=M-1
D=M-D
@EQUAL_2
D;JEQ
@SP
A=M
M=0
@END_EQ_2
0;JMP
(EQUAL_2)
@SP
A=M
M=-1
(END_EQ_2)
@SP
M=M+1
@892
D=A
@SP
A=M
M=D
@SP
M=M+1
@891
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
@SP
AM=M-1
D=M-D
@LESS_3
D;JLT
@SP
A=M
M=0
@END_LT_3
0;JMP
(LESS_3)
@SP
A=M
M=-1
(END_LT_3)
@SP
M=M+1
@891
D=A
@SP
A=M
M=D
@SP
M=M+1
@892
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
@SP
AM=M-1
D=M-D
@LESS_4
D;JLT
@SP
A=M
M=0
@END_LT_4
0;JMP
(LESS_4)
@SP
A=M
M=-1
(END_LT_4)
@SP
M=M+1
@891
D=A
@SP
A=M
M=D
@SP
M=M+1
@891
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
@SP
AM=M-1
D=M-D
@LESS_5
D;JLT
@SP
A=M
M=0
@END_LT_5
0;JMP
(LESS_5)
@SP
A=M
M=-1
(END_LT_5)
@SP
M=M+1
@32767
D=A
@SP
A=M
M=D
@SP
M=M+1
@32766
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
@SP
AM=M-1
D=M-D
@GREATER_6
D;JGT
@SP
A=M
M=0
@END_GT_6
0;JMP
(GREATER_6)
@SP
A=M
M=-1
(END_GT_6)
@SP
M=M+1
@32766
D=A
@SP
A=M
M=D
@SP
M=M+1
@32767
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
@SP
AM=M-1
D=M-D
@GREATER_7
D;JGT
@SP
A=M
M=0
@END_GT_7
0;JMP
(GREATER_7)
@SP
A=M
M=-1
(END_GT_7)
@SP
M=M+1
@32766
D=A
@SP
A=M
M=D
@SP
M=M+1
@32766
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
@SP
AM=M-1
D=M-D
@GREATER_8
D;JGT
@SP
A=M
M=0
@END_GT_8
0;JMP
(GREATER_8)
@SP
A=M
M=-1
(END_GT_8)
@SP
M=M+1
@57
D=A
@SP
A=M
M=D
@SP
M=M+1
@31
D=A
@SP
A=M
M=D
@SP
M=M+1
@53
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
M=M-1
A=M
D=M
@SP
M=M-1
A=M
M=D+M
@SP
M=M+1
@112
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
M=M-1
A=M
D=M
@SP
M=M-1
A=M
M=M-D
@SP
M=M+1
@SP
M=M-1
A=M
M=-M
@SP
M=M+1
@SP
AM=M-1
D=M
@SP
AM=M-1
M=D&M
@SP
M=M+1
@82
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
@SP
AM=M-1
M=D|M
@SP
M=M+1
@SP
AM=M-1
M=!M
@SP
M=M+1
|]

staticTest :: String
staticTest =
  [r|// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/7/StackArithmetic/StackTest/StackTest.vm

// Executes a sequence of arithmetic and logical operations on the stack. 

push constant 17
push constant 17
eq
push constant 17
push constant 16
eq
push constant 16
push constant 17
eq
push constant 892
push constant 891
lt
push constant 891
push constant 892
lt
push constant 891
push constant 891
lt
push constant 32767
push constant 32766
gt
push constant 32766
push constant 32767
gt
push constant 32766
push constant 32766
gt
push constant 57
push constant 31
push constant 53
add
push constant 112
sub
neg
and
push constant 82
or
not
|]

staticTestAsm :: String
staticTestAsm =
  [r|// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/7/StackArithmetic/StackTest/StackTest.vm
// Executes a sequence of arithmetic and logical operations on the stack.
@17
D=A
@SP
A=M
M=D
@SP
M=M+1
@17
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
@SP
AM=M-1
D=M-D
@EQUAL_0
D;JEQ
@SP
A=M
M=0
@END_EQ_0
0;JMP
(EQUAL_0)
@SP
A=M
M=-1
(END_EQ_0)
@SP
M=M+1
@17
D=A
@SP
A=M
M=D
@SP
M=M+1
@16
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
@SP
AM=M-1
D=M-D
@EQUAL_1
D;JEQ
@SP
A=M
M=0
@END_EQ_1
0;JMP
(EQUAL_1)
@SP
A=M
M=-1
(END_EQ_1)
@SP
M=M+1
@16
D=A
@SP
A=M
M=D
@SP
M=M+1
@17
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
@SP
AM=M-1
D=M-D
@EQUAL_2
D;JEQ
@SP
A=M
M=0
@END_EQ_2
0;JMP
(EQUAL_2)
@SP
A=M
M=-1
(END_EQ_2)
@SP
M=M+1
@892
D=A
@SP
A=M
M=D
@SP
M=M+1
@891
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
@SP
AM=M-1
D=M-D
@LESS_3
D;JLT
@SP
A=M
M=0
@END_LT_3
0;JMP
(LESS_3)
@SP
A=M
M=-1
(END_LT_3)
@SP
M=M+1
@891
D=A
@SP
A=M
M=D
@SP
M=M+1
@892
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
@SP
AM=M-1
D=M-D
@LESS_4
D;JLT
@SP
A=M
M=0
@END_LT_4
0;JMP
(LESS_4)
@SP
A=M
M=-1
(END_LT_4)
@SP
M=M+1
@891
D=A
@SP
A=M
M=D
@SP
M=M+1
@891
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
@SP
AM=M-1
D=M-D
@LESS_5
D;JLT
@SP
A=M
M=0
@END_LT_5
0;JMP
(LESS_5)
@SP
A=M
M=-1
(END_LT_5)
@SP
M=M+1
@32767
D=A
@SP
A=M
M=D
@SP
M=M+1
@32766
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
@SP
AM=M-1
D=M-D
@GREATER_6
D;JGT
@SP
A=M
M=0
@END_GT_6
0;JMP
(GREATER_6)
@SP
A=M
M=-1
(END_GT_6)
@SP
M=M+1
@32766
D=A
@SP
A=M
M=D
@SP
M=M+1
@32767
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
@SP
AM=M-1
D=M-D
@GREATER_7
D;JGT
@SP
A=M
M=0
@END_GT_7
0;JMP
(GREATER_7)
@SP
A=M
M=-1
(END_GT_7)
@SP
M=M+1
@32766
D=A
@SP
A=M
M=D
@SP
M=M+1
@32766
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
@SP
AM=M-1
D=M-D
@GREATER_8
D;JGT
@SP
A=M
M=0
@END_GT_8
0;JMP
(GREATER_8)
@SP
A=M
M=-1
(END_GT_8)
@SP
M=M+1
@57
D=A
@SP
A=M
M=D
@SP
M=M+1
@31
D=A
@SP
A=M
M=D
@SP
M=M+1
@53
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
M=M-1
A=M
D=M
@SP
M=M-1
A=M
M=D+M
@SP
M=M+1
@112
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
M=M-1
A=M
D=M
@SP
M=M-1
A=M
M=M-D
@SP
M=M+1
@SP
M=M-1
A=M
M=-M
@SP
M=M+1
@SP
AM=M-1
D=M
@SP
AM=M-1
M=D&M
@SP
M=M+1
@82
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
@SP
AM=M-1
M=D|M
@SP
M=M+1
@SP
AM=M-1
M=!M
@SP
M=M+1
|]
