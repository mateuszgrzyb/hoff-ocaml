#!/bin/sh

[ "$(basename $PWD)" = "ocaml" ] &&
dune exec ./main.exe > ./misc/test.ll &&
clang -S -emit-llvm ./misc/stdlib.c -o ./misc/stdlib.ll &&
clang ./misc/test.ll ./misc/stdlib.ll -o ./misc/test.x && 
echo "Compilation successfull" ||  
echo "Compilation error"
