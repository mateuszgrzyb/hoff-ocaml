
let rec generate (c: Misc.context_t) (lit: Ast.lit_t): Misc.tv_t = 
  match lit with
  | Int i -> _generate_int c i
  | Float f -> _generate_float c f 
  | Bool b -> _generate_bool c b 
  | String s -> _generate_string c s 
  | Lambda (argnames, types, expr) -> _generate_lambda c argnames types expr


and _generate_int (c: Misc.context_t) (i: int): Misc.tv_t = 
  { t = IntT
  ; v = Llvm.const_int (Misc.int_t c) i
  }

and _generate_float (c: Misc.context_t) (f: float): Misc.tv_t = 
  { t = FloatT
  ; v = Llvm.const_float (Misc.float_t c) f
  }

and _generate_bool (c: Misc.context_t) (b: bool): Misc.tv_t = 
  { t = BoolT
  ; v = Llvm.const_int (Misc.bool_t c) (if b then 1 else 0)
  }

and _generate_string (c: Misc.context_t) (s: string): Misc.tv_t = 
  let local_generate_string (): Llvm.llvalue =
    Llvm.build_global_stringptr s "" c.b 
  in 
  let global_generate_string (): Llvm.llvalue =
    Llvm.const_stringz c.c s
  in 

  let v = if c.global
    then global_generate_string ()
    else local_generate_string ()
  in

  Llvm.type_of v |> Llvm.string_of_lltype |> print_endline;
  { t = StringT
  ; v = v
  }

and _generate_lambda (_c: Misc.context_t) (_argnames: string list) (_types: Ast.type_t list) (_expr: Ast.expr_t): Misc.tv_t = 
  failwith ""