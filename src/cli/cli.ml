type args_t =
  { input_files : string list
  ; output_file : string
  ; workdir : string
  ; emit_llvm : bool
  ; no_opt : bool
  ; ext_list : string list
  }

let rec get_all_files (dir : string) : string list =
  if dir |> Sys.is_directory |> not
  then [ dir ]
  else
    dir
    |> Sys.readdir
    |> Array.to_list
    |> List.map (Filename.concat dir)
    |> List.map get_all_files
    |> List.flatten
;;

let generate_input_files (workdir : string) (files : string list) : string list =
  let files =
    if files = [] && workdir != ""
    then [ workdir ]
    else List.map (Filename.concat workdir) files
  in
  let input_files =
    files
    |> List.map get_all_files
    |> List.flatten
    |> List.filter (fun file -> Filename.extension file = ".hff")
  in
  match input_files with
  | [] -> Errors.CLIError "Input files list is empty" |> raise
  | _ -> input_files
;;

let generate_ext_list (ext_list : string list) : string list =
  ext_list |> List.map (fun ext -> if String.sub ext 0 1 = "." then ext else "." ^ ext)
;;

let parse () : args_t =
  let input_files = ref [] in
  let output_file = ref "main.x" in
  let workdir = ref "" in
  let emit_llvm = ref false in
  let no_opt = ref false in
  let ext_list = ref [ ".hff" ] in
  let pos_args arg = input_files := arg :: !input_files in
  let spec_list =
    [ "-o", Arg.Set_string output_file, "Set output file name"
    ; "-dir", Arg.Set_string workdir, "Set project workdir"
    ; "-emit-llvm", Arg.Set emit_llvm, "Emit LLVM IR"
    ; "-no-opt", Arg.Set no_opt, "Disable optimisations"
    ; "-ext", Arg.String (fun s -> ext_list := s :: !ext_list), "File extension"
    ]
  in
  let usage_msg =
    "hoff <-o output> <-dir workdir> <-emit-llvm> <-no-opt> [files or directories]"
  in
  Arg.parse spec_list pos_args usage_msg;
  { input_files = !input_files |> generate_input_files !workdir
  ; output_file = !output_file
  ; workdir = !workdir
  ; emit_llvm = !emit_llvm
  ; no_opt = !no_opt
  ; ext_list = !ext_list |> generate_ext_list
  }
;;