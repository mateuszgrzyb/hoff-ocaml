; ModuleID = 'test'
source_filename = "test"
target triple = "arm64-apple-macosx12.0.0"

%Class = type { i64, i64 (i64, i64)* }

declare i64 @read_int()

declare i1 @read_bool()

declare double @read_float()

declare i8* @read_string()

declare i64 @print_int(i64)

declare i64 @print_bool(i1)

declare i64 @print_float(double)

declare i64 @print_string(i8*)

declare i8* @GC_malloc(i32)

define i64 @main() {
entry:
  %malloccall = tail call i8* @GC_malloc(i32 ptrtoint (%Class* getelementptr (%Class, %Class* null, i32 1) to i32))
  %malloc = bitcast i8* %malloccall to %Class*
  %gep = getelementptr inbounds %Class, %Class* %malloc, i32 0, i32 0
  store i64 0, i64* %gep, align 4
  %gep1 = getelementptr inbounds %Class, %Class* %malloc, i32 0, i32 1
  store i64 (i64, i64)* @"$HOFF_LAMBDA_0", i64 (i64, i64)** %gep1, align 8
  %gep2 = getelementptr inbounds %Class, %Class* %malloc, i32 0, i32 1
  %load = load i64 (i64, i64)*, i64 (i64, i64)** %gep2, align 8
  %callexpr = call i64 %load(i64 1, i64 2)
  %callexpr3 = call i64 @print_int(i64 %callexpr)
  ret i64 0
}

declare noalias i8* @malloc(i32)

define i64 @"$HOFF_LAMBDA_0"(i64 %i, i64 %j) {
entry:
  %addexpr = add i64 %i, %j
  ret i64 %addexpr
}

