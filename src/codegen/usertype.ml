
let rec generate (c: Misc.context_t) (name: string) (user_type: Ast.user_type_t): Llvm.lltype =
  match user_type with
  | Alias (type_) -> _generate_alias c type_
  | ADT (constructors) -> _generate_adt c name constructors

and _generate_alias (c: Misc.context_t) (type_: Ast.type_t): Llvm.lltype = 
  Misc.get_llvm_type c type_


and _generate_adt (c: Misc.context_t) (name: string) (constructors: Ast.constructor_t list): Llvm.lltype = 
  let llvm_constructors = List.map (_generate_constructor c name) constructors in 
  let llvm_type = Llvm.named_struct_type c.c name in 
  Llvm.struct_set_body llvm_type [|List.hd llvm_constructors|] false;
  llvm_type


and _generate_constructor (c: Misc.context_t) (name: string) (constructor: Ast.constructor_t): Llvm.lltype = 
  let cname, types = constructor in
  let llvm_struct = Llvm.named_struct_type c.c (Helpers.constructor_name [name; cname]) in
  Llvm.struct_set_body llvm_struct (Array.of_list (List.map (Misc.get_llvm_type c) types)) false;
  llvm_struct