
class ['v] namespace = object
  val _tbl = Hashtbl.create 9
  
  method add (name: string) (value: 'v): unit =
    Hashtbl.add _tbl name value
  method remove (name: string): unit = 
    Hashtbl.remove _tbl name
  method get (name: string): 'v = 
    try
      Hashtbl.find _tbl name
    with Not_found -> raise 
      (Errors.TypeError (Printf.sprintf "User defined type %s not found" name))

end


type context_t = 
  { c: Llvm.llcontext
  ; m: Llvm.llmodule
  ; b: Llvm.llbuilder
  ; names: Llvm.llvalue namespace
  ; types: Llvm.lltype namespace
  ; mutable global: bool
  }

type tv_t = 
  { t: Ast.type_t
  ; v: Llvm.llvalue
  }

module type Node = sig
  type t
  val generate: context_t -> t -> tv_t
end

let initialize (name: string): context_t = 
  let context = Llvm.global_context () in
  { c = context
  ; m = Llvm.create_module context name
  ; b = Llvm.builder context
  ; names = new namespace
  ; types = new namespace
  ; global = true
  }

let rec int_t (c: context_t): Llvm.lltype = Llvm.i32_type c.c

and float_t (c: context_t): Llvm.lltype = Llvm.float_type c.c

and bool_t (c: context_t): Llvm.lltype = Llvm.i1_type c.c

and string_t (c: context_t): Llvm.lltype = (Llvm.i8_type c.c) |> Llvm.pointer_type


and fun_t (c: context_t) (ts: Ast.type_t list): Llvm.lltype = 
  let llvm_t (t: Ast.type_t) = get_llvm_type c t in 
  let rec split (l: 'a list): ('a * ('a list)) = 
    match l with 
    | [] -> failwith "init"
    | [x] -> (x, [])
    | x :: xs -> 
      let l, ls = split xs in (l, x :: ls) in
  let rt, ts = split ts in
  Llvm.function_type (llvm_t rt) (Array.of_list (List.map llvm_t ts))

and get_llvm_type (c: context_t) (type_: Ast.type_t): Llvm.lltype = 
  match type_ with
  | IntT -> int_t c
  | FloatT -> float_t c
  | BoolT -> bool_t c
  | StringT -> string_t c
  | FunT ts -> fun_t c ts |> Llvm.pointer_type
  | UserT name -> c.types#get name