
let rec generate (c: Misc.context_t) (lit: Ast.lit_t): Misc.tv_t = 
  match lit with
  | Int i -> _generate_int c i
  | Float f -> _generate_float c f 
  | Bool b -> _generate_bool c b 
  | String s -> _generate_string c s 
  | Lambda (argnames, types, expr) -> _generate_lambda c argnames types expr


and _generate_int (c: Misc.context_t) (i: int): Misc.tv_t = 
  { t = IntT
  ; v = Llvm.const_int Misc.int_t i
  }

and _generate_float (c: Misc.context_t) (f: float): Misc.tv_t = 
  { t = FloatT
  ; v = Llvm.const_float Misc.float_t f
  }

and _generate_bool (c: Misc.context_t) (b: bool): Misc.tv_t = 
  { t = BoolT
  ; v = Llvm.const_int Misc.bool_t (if b then 1 else 0)
  }

and _generate_string (c: Misc.context_t) (s: string): Misc.tv_t = 
  { t = StringT
  ; v = Llvm.build_global_string s "string_name" c.b
  }

and _generate_lambda (c: Misc.context_t) (argnames: string list) (types: Ast.type_t list) (expr: Ast.expr_t): Misc.tv_t = 
  ()