




let test (): unit =
  let c = Misc.initialize "test" in 

  let ft = Llvm.function_type (Misc.int_t c) [|Misc.int_t c; Misc.int_t c|] in

  let _f1 = Llvm.declare_function "fun1" ft c.m in 
  let _f2 = Llvm.declare_function "fun2" ft c.m in 

  let llvm_bb = Llvm.append_block c.c "entry" _f1 in 
  Llvm.position_at_end llvm_bb c.b;
  (*
  let body_tv = Expr.generate c body in
  *)


  ignore (Llvm.build_ret (Llvm.const_int (Misc.int_t c) 1) c.b);


  Llvm_analysis.assert_valid_module c.m;
  Llvm.dump_module c.m
 
let () = Compiler.run ()
(*
let () = test ()
*)
