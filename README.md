# Forthc - a retargetable forth compiler
This project was mainly intended as a port of FORTH to the
[D16](https://github.com/d16-processor/d16), however it should be
easily retargetable to another processor by changing src/D16Asm.hs to
the target processor.

## Building
First install Haskell's `stack`, then run
    
	stack build
	stack exec forthc-exe -- file.fs file.asm

This will build the compile and compile `file.fs` to `file.asm`, which
can then be assembled and linked by the d16 assembler and
[linker](https://github.com/d16-processor/d16-ld).


