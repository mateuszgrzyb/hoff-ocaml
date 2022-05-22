; ModuleID = 'test'
source_filename = "test"
target triple = "arm64-apple-macosx12.0.0"

%Student = type { %Student.0 }
%Student.0 = type { %Student*, i64*, i64* }

declare i64 @read_int()

declare i1 @read_bool()

declare double @read_float()

declare i8* @read_string()

declare i64 @print_int(i64)

declare i64 @print_bool(i1)

declare i64 @print_float(double)

declare i64 @print_string(i8*)

define i64 @test1() {
entry:
  %callexpr = call i64 @"$test1$local1$"()
  ret i64 %callexpr
}

define i64 (i64, i64)* @test3() {
entry:
  ret i64 (i64, i64)* @"$test3$local1$"
}

define i64 @test4(i64 (i64, i64)* %f) {
entry:
  %callexpr = call i64 %f(i64 3, i64 4)
  ret i64 %callexpr
}

define i64 @"$HOFF_OVERLOADED_OPERATOR_..."(i64 %i, i64 %j) {
entry:
  %subexpr = sub i64 %i, %j
  ret i64 %subexpr
}

define i64 @what(%Student %s) {
entry:
  ret i64 23
}

define i64 @main() {
entry:
  %callexpr = call i64 (i64, i64)* @test3()
  %callexpr1 = call i64 @test4(i64 (i64, i64)* %callexpr)
  %callexpr2 = call i64 @print_int(i64 %callexpr1)
  %callexpr3 = call i64 @"$HOFF_OVERLOADED_OPERATOR_..."(i64 3, i64 5)
  %callexpr4 = call i64 @print_int(i64 %callexpr3)
  ret i64 0
}

define i64 @"$test1$local1$"() {
entry:
  %callexpr = call i64 @"$test1$local2$"()
  ret i64 %callexpr
}

define i64 @"$test1$local2$"() {
entry:
  %callexpr = call i64 @"$test1$local1$"()
  ret i64 %callexpr
}

define i64 @"$test1$local1$local3$"() {
entry:
  ret i64 0
}

define i64 @"$test3$local1$"(i64 %i, i64 %j) {
entry:
  %addexpr = add i64 %i, %j
  ret i64 %addexpr
}

