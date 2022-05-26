; ModuleID = 'test'
source_filename = "test"
target triple = "arm64-apple-macosx12.0.0"

%Class = type { i64, i64 (i64, i64)* }
%Student = type { i64, i64 }

@string = private unnamed_addr constant [9 x i8] c"FizzBuzz\00", align 1
@string.1 = private unnamed_addr constant [5 x i8] c"Buzz\00", align 1
@string.2 = private unnamed_addr constant [5 x i8] c"Fizz\00", align 1
@string.3 = private unnamed_addr constant [23 x i8] c"What is your fizzbuzz?\00", align 1
@string.4 = private unnamed_addr constant [12 x i8] c"Fizzbuzzing\00", align 1

declare i64 @read_int()

declare i1 @read_bool()

declare double @read_float()

declare i8* @read_string()

declare i64 @print_int(i64)

declare i64 @print_bool(i1)

declare i64 @print_float(double)

declare i64 @print_string(i8*)

declare i8* @GC_malloc(i64)

define i64 @add(i64 %i, i64 %j) {
entry:
  %addexpr = add i64 %i, %j
  ret i64 %addexpr
}

; Function Attrs: alwaysinline
define %Class* @"$HOFF_GLOBAL_class"() #0 {
entry:
  %malloccall = tail call i8* @malloc(i32 ptrtoint (%Class* getelementptr (%Class, %Class* null, i32 1) to i32))
  %malloc = bitcast i8* %malloccall to %Class*
  %gep = getelementptr inbounds %Class, %Class* %malloc, i32 0, i32 0
  store i64 1, i64* %gep, align 4
  %gep1 = getelementptr inbounds %Class, %Class* %malloc, i32 0, i32 1
  store i64 (i64, i64)* @add, i64 (i64, i64)** %gep1, align 8
  ret %Class* %malloc
}

; Function Attrs: alwaysinline
define i64 (i64, i64)* @"$HOFF_GLOBAL_adder_from_class"() #0 {
entry:
  %globalvalue = call %Class* @"$HOFF_GLOBAL_class"()
  %gep = getelementptr inbounds %Class, %Class* %globalvalue, i32 0, i32 1
  %load = load i64 (i64, i64)*, i64 (i64, i64)** %gep, align 8
  ret i64 (i64, i64)* %load
}

define %Student* @test() {
entry:
  %malloccall = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2) to i32))
  %malloc = bitcast i8* %malloccall to %Student*
  %gep = getelementptr inbounds %Student, %Student* %malloc, i32 0, i32 0
  store i64 1, i64* %gep, align 4
  %gep1 = getelementptr inbounds %Student, %Student* %malloc, i32 0, i32 1
  store i64 2, i64* %gep1, align 4
  ret %Student* %malloc
}

define i64 @get_from_student(%Student* %s) {
entry:
  %gep = getelementptr inbounds %Student, %Student* %s, i32 0, i32 1
  %load = load i64, i64* %gep, align 4
  ret i64 %load
}

define i64 @fizzbuzz(i64 %i) {
entry:
  %leexpr = icmp sle i64 %i, 0
  br i1 %leexpr, label %thenblock, label %elseblock

thenblock:                                        ; preds = %entry
  br label %fiblock21

elseblock:                                        ; preds = %entry
  %remexpr = srem i64 %i, 5
  %eqexpr = icmp eq i64 %remexpr, 0
  %remexpr1 = srem i64 %i, 3
  %eqexpr2 = icmp eq i64 %remexpr1, 0
  %andexpr = and i1 %eqexpr, %eqexpr2
  br i1 %andexpr, label %thenblock3, label %elseblock4

thenblock3:                                       ; preds = %elseblock
  %callexpr = call i64 @print_string(i8* getelementptr inbounds ([9 x i8], [9 x i8]* @string, i32 0, i32 0))
  br label %fiblock18

elseblock4:                                       ; preds = %elseblock
  %remexpr5 = srem i64 %i, 5
  %eqexpr6 = icmp eq i64 %remexpr5, 0
  br i1 %eqexpr6, label %thenblock7, label %elseblock9

thenblock7:                                       ; preds = %elseblock4
  %callexpr8 = call i64 @print_string(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @string.1, i32 0, i32 0))
  br label %fiblock16

elseblock9:                                       ; preds = %elseblock4
  %remexpr10 = srem i64 %i, 3
  %eqexpr11 = icmp eq i64 %remexpr10, 0
  br i1 %eqexpr11, label %thenblock12, label %elseblock14

thenblock12:                                      ; preds = %elseblock9
  %callexpr13 = call i64 @print_string(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @string.2, i32 0, i32 0))
  br label %fiblock

elseblock14:                                      ; preds = %elseblock9
  %callexpr15 = call i64 @print_int(i64 %i)
  br label %fiblock

fiblock:                                          ; preds = %elseblock14, %thenblock12
  %phi = phi i64 [ %callexpr13, %thenblock12 ], [ %callexpr15, %elseblock14 ]
  br label %fiblock16

fiblock16:                                        ; preds = %fiblock, %thenblock7
  %phi17 = phi i64 [ %callexpr8, %thenblock7 ], [ %phi, %fiblock ]
  br label %fiblock18

fiblock18:                                        ; preds = %fiblock16, %thenblock3
  %phi19 = phi i64 [ %callexpr, %thenblock3 ], [ %phi17, %fiblock16 ]
  %subexpr = sub i64 %i, 1
  %callexpr20 = call i64 @fizzbuzz(i64 %subexpr)
  br label %fiblock21

fiblock21:                                        ; preds = %fiblock18, %thenblock
  %phi22 = phi i64 [ 0, %thenblock ], [ %callexpr20, %fiblock18 ]
  ret i64 %phi22
}

define i64 @main() {
entry:
  %callexpr = call i64 @print_string(i8* getelementptr inbounds ([23 x i8], [23 x i8]* @string.3, i32 0, i32 0))
  %callexpr1 = call i64 @read_int()
  %callexpr2 = call i64 @print_string(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @string.4, i32 0, i32 0))
  %callexpr3 = call i64 @fizzbuzz(i64 %callexpr1)
  %malloccall = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2) to i32))
  %malloc = bitcast i8* %malloccall to %Student*
  %gep = getelementptr inbounds %Student, %Student* %malloc, i32 0, i32 0
  store i64 4, i64* %gep, align 4
  %gep4 = getelementptr inbounds %Student, %Student* %malloc, i32 0, i32 1
  store i64 5, i64* %gep4, align 4
  %callexpr5 = call i64 @get_from_student(%Student* %malloc)
  %callexpr6 = call i64 @print_int(i64 %callexpr5)
  ret i64 0
}

declare noalias i8* @malloc(i32)

attributes #0 = { alwaysinline }

