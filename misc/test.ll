; ModuleID = 'test'
source_filename = "test"
target triple = "arm64-apple-macosx12.0.0"

@v1 = global i64 1

declare i64 @read_int()

declare i1 @read_bool()

declare double @read_float()

declare i8* @read_string()

declare i64 @print_int(i64)

declare i64 @print_bool(i1)

declare i64 @print_float(double)

declare i64 @print_string(i8*)

define i64 @f1(i64 %i, i64 %j) {
entry:
  %addexpr = add i64 %i, %j
  ret i64 %addexpr
}

define i64 @f2(i64 %i, i64 %j) {
entry:
  ret i64 1
}

define i64 @main() {
entry:
  ret i64 0
}

