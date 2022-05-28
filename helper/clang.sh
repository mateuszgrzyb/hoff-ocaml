#!/bin/sh

BOEHMGC_PATH='/opt/homebrew/Cellar/bdw-gc/8.0.6'

IR_FILE=$1
OUTPUT_FILE=$2

clang -S -emit-llvm ./stdlib/io.c -o stdlib.ll &&
clang "$IR_FILE" stdlib.ll \
    -Wno-override-module \
    -I$BOEHMGC_PATH/include/ \
	-L$BOEHMGC_PATH/lib/ -lgc \
    -o "$OUTPUT_FILE"

rm stdlib.ll "$IR_FILE"