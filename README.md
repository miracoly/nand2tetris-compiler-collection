# Hack Assembler - Nand2Tetris

[Nand2Tetris](https://www.nand2tetris.org/) is a 12-week online course [available at Coursera](https://www.coursera.org/learn/build-a-computer) which teaches the fundamentals of computer architecture and low-level programming.
Week 6 is about programming an assembler for the previous build architecture, called the Hack computer. This is my solution...

## Steps
1. Remove whitespaces and comments
2. Scan for symbols and fill LookUp table
3. Translate each line
   - `A` Instrcution or
   - `C` Instruction

## Limitations
- works only with decimal numbers in assembly