

type context_t = 
  { c: Llvm.llcontext
  ; m: Llvm.llmodule
  ; b: Llvm.llbuilder
  ; names: (string, string) Hashtbl.t
  ; types: (string, string) Hashtbl.t
  }

type tv_t = 
  { t: Ast.type_t
  ; v: Llvm.llvalue
  }

module type Node = sig
  type t
  val generate: t -> tv_t
end

let initialize (name: string): context_t = 
  let context = Llvm.global_context () in
  { c = context
  ; m = Llvm.create_module context name
  ; b = Llvm.builder context
  ; names = Hashtbl.create 9
  ; types = Hashtbl.create 9
  }