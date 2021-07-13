
type result_t = 
  | Tokens | SyntaxTree | LlvmIr | Exe
  [@@deriving show]

type args_t = 
  { result: result_t
  ; output: string
  ; modules: string list
  } 
  [@@deriving show]

let parse_args (): args_t = 
  (*
  let usage = Printf.sprintf "Usage: %s [options]" ((String.split_on_char '/' Sys.argv.(0)) |> List.rev |> List.hd) in
  *)
  let usage = Printf.sprintf "Usage: %s [options]" Sys.argv.(0) in
  let result = ref LlvmIr in
  let output = ref "main.x" in 
  let modules = ref [] in

  let speclist = [
    ("-mode", Arg.String (fun mode ->
      result := match mode with
      | "tokens" -> Tokens
      | "syntaxtree" -> SyntaxTree 
      | "llvmir" -> LlvmIr 
      | "exe" ->  Exe
      | unknwn -> raise (Arg.Bad unknwn)), "[tokens|syntaxtree|llvmir|exe] Set compilation result");
    ("-o", Arg.Set_string output, "<output> Set output filename");
  ] in

  let anon_fun m = modules := m::!modules in

  Arg.parse speclist anon_fun usage;
  { result = LlvmIr
  ; output = !output
  ; modules = !modules
  }