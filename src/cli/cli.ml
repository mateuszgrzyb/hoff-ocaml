open Core
open Printf

type ext_t =
  | Hoff
  | IR

type file_t =
  { name : string
  ; ext : ext_t
  }

type args_t =
  { input_files : file_t list
  ; output_file : string
  ; workdir : string
  ; emit_llvm : bool
  ; no_opt : bool
  }

let rec get_all_files (dir : string) : string list =
  match dir |> Sys.is_directory with
  | `No -> [ dir ]
  | _ ->
    dir
    |> Sys.readdir
    |> Array.to_list
    |> List.map ~f:(Filename.concat dir)
    |> List.map ~f:get_all_files
    |> List.concat
;;

let generate_input_files (workdir : string) (files : string list) : file_t list =
  let files =
    if phys_equal files [] && not (phys_equal workdir "")
    then [ workdir ]
    else List.map ~f:(Filename.concat workdir) files
  in
  let is_hoff_file (file : string) : file_t option =
    Printf.eprintf "file: %s\n" file;
    Printf.eprintf
      "ext: %s\n"
      (file |> Filename.split_extension |> snd |> fun s -> Option.value s ~default:"none");
    match file |> Filename.split_extension |> snd with
    | Some "hff" -> Some { name = file; ext = Hoff }
    | Some "ir" -> Some { name = file; ext = IR }
    | _ -> None
  in
  let input_files =
    files |> List.map ~f:get_all_files |> List.concat |> List.filter_map ~f:is_hoff_file
  in
  match input_files with
  | [] ->
    files
    |> String.concat ~sep:"\n"
    |> sprintf "Input files list is empty.\nWorkdir: %s\nInput files: \n%s\n" workdir
    |> fun s -> Errors.CLIError s |> raise
  | _ -> input_files
;;

let generate_ext_list (ext_list : string list) : string list =
  ext_list
  |> List.map ~f:(fun ext ->
         if phys_equal (String.slice ext 0 1) "." then ext else "." ^ ext)
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
  }
;;