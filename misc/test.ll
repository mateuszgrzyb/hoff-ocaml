; ModuleID = 'test'
source_filename = "test"
target triple = "x86_64-pc-linux-gnu"

declare i16 @read_int()

declare i1 @read_bool()

declare float @read_float()

declare i16 @print_int(i16)

declare i16 @print_bool(i1)

declare i16 @print_float(float)

define i16 @mapInt(i16 (i16)* %f, i16 %i) {
entry:
  %callexpr = call i16 %f(i16 %i)
  ret i16 %callexpr
}

define i16 @succInt(i16 %i) {
entry:
  %callexpr = call i16 @mapInt(i16 (i16)* @HOFF_LAMBDA_0, i16 %i)
  ret i16 %callexpr
}

define i16 @HOFF_LAMBDA_0(i16 %j) {
entry:
  ret i16 2
}

