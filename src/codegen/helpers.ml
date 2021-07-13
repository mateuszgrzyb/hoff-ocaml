

let rec drop_n (n: int) (l: 'a list): 'a list = 
  if n <= 0 then l else 
    match l with 
    | [] -> []
    | _ :: ls -> drop_n (n-1) ls

let rec take_n (n: int) (l: 'a list): 'a list = 
  if n <= 0 then [] else
    match l with 
    | [] -> []
    | x :: xs -> x :: take_n (n-1) xs

let rec zip3 (l1: 'a list) (l2: 'b list) (l3: 'c list): ('a * 'b * 'c) list = 
  match (l1, l2, l3) with
  | ([], _, _) | (_, [], _) | (_, _, []) -> []
  | (x::xs, y::ys, z::zs) -> ((x, y, z) :: zip3 xs ys zs)

let name_mangler (name: string): (string list -> string) = 
  let mangler (ids: string list): string = 
    "?" ^ (String.concat "_" (name :: ids))
  in 
    mangler


let lambda_name = name_mangler "ANONYMOUSFUNCTION"
let local_name = name_mangler "LOCALFUNCTION"

let const_name = name_mangler "CONSTEXPR"
let constructor_name = name_mangler "CONSTRUCTOR"

let generate_funcpredecl
  (c: Misc.context_t) 
  (name: string)
  (types: Ast.type_t list)
  : Misc.tv_t = 
  let llvm_function_type = Misc.fun_t c types in
  let llvm_function = Llvm.declare_function name llvm_function_type c.m in 

  (* typecheck *)
  (*
  let return_type = match drop_n (List.length argnames) types with
    | [] -> raise (Errors.TypeError "")
    | [rt] -> rt
    | ts -> Ast.FunT ts in
  if Misc.compare_types c body_tv.t return_type 
    then raise (Errors.TypeError "");
  *)

  { t = FunT types
  ; v = llvm_function
  }

let generate_funcdef
  (expr_generate: Misc.context_t -> Ast.expr_t -> Misc.tv_t)
  (c: Misc.context_t)
  (llvm_function: Llvm.llvalue)
  (argnames: string list)
  (types: Ast.type_t list)
  (body: Ast.expr_t)
  : Misc.tv_t = 
  
  List.iter (fun (n, v, t) ->
    c.names#add n { v=v; t=t }
  ) (zip3 argnames (Array.to_list (Llvm.params llvm_function)) types);

  
  let llvm_bb = Llvm.append_block c.c "entry" llvm_function in 
  Llvm.position_at_end llvm_bb c.b;
  (*
  let body_tv = Expr.generate c body in
  *)
  let body_tv = expr_generate c body in

  print_endline ("its over");

  ignore (Llvm.build_ret body_tv.v c.b);
  
    
  List.iter (fun n ->
    c.names#remove n 
  ) argnames;

  (* typecheck *)
  (*
  let return_type = match drop_n (List.length argnames) types with
    | [] -> raise (Errors.TypeError "")
    | [rt] -> rt
    | ts -> Ast.FunT ts in
  if Misc.compare_types c body_tv.t return_type 
    then raise (Errors.TypeError "");
  *)
  { t = FunT types
  ; v = llvm_function
  }

let generate_funcdecl
  (expr_generate: Misc.context_t -> Ast.expr_t -> Misc.tv_t)
  (c: Misc.context_t) 
  (name: string)
  (argnames: string list)
  (types: Ast.type_t list)
  (body: Ast.expr_t)
  : Misc.tv_t = 

  let llvm_function_type = Misc.fun_t c types in
  let llvm_function = Llvm.declare_function name llvm_function_type c.m in 

  List.iter (fun (n, v, t) ->
    c.names#add n { v=v; t=t }
  ) (zip3 argnames (Array.to_list (Llvm.params llvm_function)) types);

  let llvm_bb = Llvm.append_block c.c "entry" llvm_function in 
  Llvm.position_at_end llvm_bb c.b;
  (*
  let body_tv = Expr.generate c body in
  *)
  let body_tv = expr_generate c body in
  ignore (Llvm.build_ret body_tv.v c.b);
    
  List.iter (fun n ->
    c.names#remove n 
  ) argnames;

  (* typecheck *)
  (*
  let return_type = match drop_n (List.length argnames) types with
    | [] -> raise (Errors.TypeError "")
    | [rt] -> rt
    | ts -> Ast.FunT ts in
  if Misc.compare_types c body_tv.t return_type 
    then raise (Errors.TypeError "");
  *)

  { t = FunT types
  ; v = llvm_function
  }

let generate_constdecl
  (expr_generate)
  (c: Misc.context_t)
  (name: string)
  (type_: Ast.type_t)
  (expr: Ast.expr_t)
  : Misc.tv_t = 
  generate_funcdecl expr_generate c (const_name [name]) [] [type_] expr


