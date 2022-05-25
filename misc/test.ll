; ModuleID = 'test'
source_filename = "test"
target triple = "arm64-apple-macosx12.0.0"

@string = private unnamed_addr constant [12 x i8] c"ala ma kota\00", align 1

declare i64 @read_int()

declare i1 @read_bool()

declare double @read_float()

declare i8* @read_string()

declare i64 @print_int(i64)

declare i64 @print_bool(i1)

declare i64 @print_float(double)

declare i64 @print_string(i8*)

define double @a() {
entry:
  ret double 6.680000e+01
}

define i64 @main() {
entry:
  %callexpr = call i64 @print_string(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @string, i32 0, i32 0))
  %callexpr1 = call i64 @print_int(i64 49)
  ret i64 0
}

