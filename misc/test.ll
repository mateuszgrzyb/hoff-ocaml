; ModuleID = 'test'
source_filename = "test"
target triple = "x86_64-pc-linux-gnu"

@globalVal0 = global i16 33

declare i16 @read_int()

declare i1 @read_bool()

declare float @read_float()

declare i16 @print_int(i16)

declare i16 @print_bool(i1)

declare i16 @print_float(float)

; Function Attrs: alwaysinline
define i16 @HOFF_GLOBAL_globalVal1() #0 {
entry:
  %callexpr = call i16 @something(i16 32)
  %divexpr = sdiv i16 %callexpr, 33
  ret i16 %divexpr
}

; Function Attrs: alwaysinline
define i16 @HOFF_GLOBAL_globalVal2() #0 {
entry:
  %globalvalue = call i16 @HOFF_GLOBAL_globalVal1()
  %subexpr = sub i16 %globalvalue, 2
  ret i16 %subexpr
}

define i16 @fibonacci(i16 %i) {
entry:
  %ltexpr = icmp slt i16 %i, 2
  br i1 %ltexpr, label %thenblock, label %elseblock

thenblock:                                        ; preds = %entry
  br label %fiblock

elseblock:                                        ; preds = %entry
  %subexpr = sub i16 %i, 1
  %callexpr = call i16 @fibonacci(i16 %subexpr)
  %subexpr1 = sub i16 %i, 2
  %callexpr2 = call i16 @fibonacci(i16 %subexpr1)
  %addexpr = add i16 %callexpr, %callexpr2
  br label %fiblock

fiblock:                                          ; preds = %elseblock, %thenblock
  %phi = phi i16 [ %i, %thenblock ], [ %addexpr, %elseblock ]
  ret i16 %phi
}

define i16 @main() {
entry:
  %callexpr = call i16 @read_int()
  %callexpr1 = call i16 @something(i16 %callexpr)
  %callexpr2 = call i16 @"HOFF_OVERLOADED_OPERATOR_***"(i16 %callexpr1, i16 3)
  %callexpr3 = call i16 @print_int(i16 %callexpr2)
  ret i16 %callexpr3
}

define i16 @f1(i16 %i) {
entry:
  %callexpr = call i16 @f2(i16 %i, i16 %i)
  %globalvalue = call i16 @HOFF_GLOBAL_globalVal1()
  %addexpr = add i16 %callexpr, %globalvalue
  ret i16 %addexpr
}

define i16 @f2(i16 %i, i16 %j) {
entry:
  %addexpr = add i16 %i, %j
  %addexpr1 = add i16 %addexpr, 33
  ret i16 %addexpr1
}

define i16 @something(i16 %i) {
entry:
  %addexpr = add i16 %i, 32
  ret i16 %addexpr
}

define i16 @"HOFF_OVERLOADED_OPERATOR_***"(i16 %i, i16 %j) {
entry:
  %addexpr = add i16 %i, %j
  %globalvalue = load i16, i16* @globalVal0, align 2
  %addexpr1 = add i16 %addexpr, %globalvalue
  ret i16 %addexpr1
}

define float @"HOFF_OVERLOADED_OPERATOR_=>"(float %f, float %g) {
entry:
  %addexpr = fadd float %f, %g
  ret float %addexpr
}

attributes #0 = { alwaysinline }

