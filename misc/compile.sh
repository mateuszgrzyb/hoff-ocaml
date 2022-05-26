#!/bin/sh

BOEHMGC_PATH='/opt/homebrew/Cellar/bdw-gc/8.0.6'

dune exec ./main.exe > ./misc/test.ll &&
clang -S -emit-llvm ./misc/stdlib.c -o ./misc/stdlib.ll &&
clang ./misc/test.ll ./misc/stdlib.ll \
    -I$BOEHMGC_PATH/include/ \
	-L$BOEHMGC_PATH/lib/ -lgc \
    -o ./misc/test.x && 
echo "Compilation successfull"
