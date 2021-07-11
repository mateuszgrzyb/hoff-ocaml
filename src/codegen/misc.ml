
open Printf

class ['v] namespace (label: string) (error: (string -> exn)) = object
  val label = label
  val error = error
  val _tbl = Hashtbl.create 9
  
  method add (name: string) (value: 'v): unit =
    Hashtbl.add _tbl name value
  method remove (name: string): unit = 
    Hashtbl.remove _tbl name
  method get (name: string): 'v = 
    try
      Hashtbl.find _tbl name
    with Not_found -> raise (error (sprintf "%s %s not found" label name))
end

type tv_t = 
  { t: Ast.type_t
  ; v: Llvm.llvalue
  }

type context_t = 
  { c: Llvm.llcontext
  ; m: Llvm.llmodule
  ; b: Llvm.llbuilder
  ; names: tv_t namespace
  ; types: Llvm.lltype namespace
  ; mutable global: bool
  }

  module type Node = sig
  type t
  val generate: context_t -> t -> tv_t
end

let initialize (name: string): context_t = 
  let context = Llvm.create_context () in
  { c = context
  ; m = Llvm.create_module context name
  ; b = Llvm.builder context
  ; names = new namespace "Name" (fun s -> Errors.NameError s)
  ; types = new namespace "Type" (fun s -> Errors.TypeError s)
  ; global = true
  }

let finalize (c: context_t): unit = 
  Llvm.dispose_module c.m;
  Llvm.dispose_context c.c

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


let compare_types (c: context_t) (t1: Ast.type_t) (t2: Ast.type_t): bool =
  let extract_user_t: Ast.type_t -> Llvm.lltype = function
    | Ast.UserT name -> c.types#get name
    | t -> get_llvm_type c t
  in
    let (t1, t2) = (extract_user_t t1, extract_user_t t2) in
    t1 <> t2