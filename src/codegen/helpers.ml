
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

let rec zip (l1: 'a list) (l2: 'b list): ('a * 'b) list = 
  match (l1, l2) with
  | ([], _) | (_, []) -> []
  | (x :: xs, y :: ys) -> ((x, y) :: zip xs ys)

let generate_funcdecl 
  (c: Misc.context_t) 
  (name: string)
  (argnames: string list)
  (types: Ast.type_t list)
  (body: Ast.expr_t)
  : Misc.tv_t = 

  let llvm_function_type = Misc.fun_t c types in
  let llvm_function = Llvm.declare_function name llvm_function_type c.m in 

  List.iter (fun (n, p) ->
    c.names#add n p
  ) (List.combine argnames (Array.to_list (Llvm.params llvm_function)));

  let bb = Llvm.append_block c.c "entry" llvm_function in 
  Llvm.position_at_end bb c.b;
  let body_tv = Expr.generate c body in
  ignore (Llvm.build_ret body_tv.v c.b);
    
  List.iter (fun n ->
    c.names#remove n 
  ) argnames;

  (* typecheck *)
  let return_type = match drop_n (List.length argnames) types with
    | [] -> raise (Errors.TypeError "")
    | [rt] -> rt
    | ts -> Ast.FunT ts in
  if body_tv.t <> return_type 
    then raise (Errors.TypeError "");


  { t = FunT types
  ; v = llvm_function
  }