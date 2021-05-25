# Hoff

Using menhir, ocamllex (probably will change into sedlex because of unicode support) and llvm

## Example
Example syntax is shown in file "misc/test.hff"

## Done
* operator overloading (without setting precedences, for now)
* let, if expressions
* primitive types (Int, Float, Bool, String)
* typechecker
* local functions
* predeclaration for every function in given scope

## Todo
* case expression
* adts
* fix lambdas
* refactor
* strings, arrays, tuples and other composite types
* io
* clojures
* global values declared as functions because global initializers must be constant values. find better solution or add optimizer just for literal values
* integer overflow (automatic type promotion/compiler errors) !!!!!!!

## Bugs
* change ast to be more "functionally oriented": curried functions, etc

