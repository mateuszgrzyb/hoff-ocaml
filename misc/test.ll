; ModuleID = 'test'
source_filename = "test"
target triple = "x86_64-pc-linux-gnu"

declare i16 @read_int()

declare i1 @read_bool()

declare float @read_float()

declare i16 @print_int(i16)

declare i16 @print_bool(i1)

declare i16 @print_float(float)

define i16 @something(i16 %a, i16 %b) {
entry:
  %addexpr = add i16 %a, %b
  ret i16 %addexpr
}

define i16 @something2(i16 %a, i16 %b) {
entry:
  %callexpr = call i16 @HOFF_LOCAL_0(i16 %a, i16 %b)
  ret i16 %callexpr
}

define i16 @something3(i16 %a, i16 %b, i16 (i16, i16)* %adder) {
entry:
  %callexpr = call i16 %adder(i16 %a, i16 %b)
  ret i16 %callexpr
}

define i16 @globalAdder(i16 %a, i16 %b) {
entry:
  %addexpr = add i16 %a, %b
  ret i16 %addexpr
}

define i16 @main() {
entry:
  %callexpr = call i16 @something(i16 3, i16 4)
  %callexpr1 = call i16 @print_int(i16 %callexpr)
  %callexpr2 = call i16 @something2(i16 3, i16 4)
  %callexpr3 = call i16 @print_int(i16 %callexpr2)
  %callexpr4 = call i16 @something3(i16 3, i16 4, i16 (i16, i16)* @globalAdder)
  %callexpr5 = call i16 @print_int(i16 %callexpr4)
  ret i16 333
}

define i16 @HOFF_LOCAL_0(i16 %a1, i16 %b1) {
entry:
  %addexpr = add i16 %a1, %b1
  ret i16 %addexpr
}

