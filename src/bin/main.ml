module Fmt = CCFormat
module A = Imandra_ast

let main lang in_paths out_path =
  (* parse input files *)
  let decls =
    CCList.flat_map
      (fun f ->
        let l =
          try Yojson.Safe.from_file f with
          | Sys_error msg ->
            Fmt.eprintf "%s@." msg;
            exit 1
          | Yojson.Json_error msg ->
            Fmt.eprintf "JSON error: %s@." msg;
            exit 1
        in

        match A.Decl.list_of_of_yojson l with
        | Ok l -> l
        | Error err ->
          Fmt.eprintf "could not decode AST: %s@." err;
          exit 2)
      in_paths
  in

  Fmt.eprintf "parsed %d declarations@." (List.length decls);

  match lang with
  | `Rust ->
    let code = Imandra_ast_cg_rust.codegen decls in
    (match out_path with
    | Some out_path -> CCIO.File.write_exn out_path code
    | None -> print_endline code)
  | `OCaml -> print_endline "OCaml code generation not implemented"

open Cmdliner

let _ =
  let languages = [ ("ocaml", `OCaml); ("rust", `Rust) ] in
  let default_language = `OCaml in
  let languages_s =
    String.concat ", "
      (List.map (fun (lang, _) -> Printf.sprintf "%S" lang) languages)
  in
  let default_language_s, _ =
    List.find (fun (_, l) -> l = default_language) languages
  in

  let cmd =
    let doc =
      "from a JSON description of Imandra IML values, generate OCaml or Rust \
       code for {de}serializing those values"
    in
    let lang =
      let doc =
        Printf.sprintf
          "the target language (either %s, where the default is %S)" languages_s
          default_language_s
      in
      Arg.(
        required
        & opt (some (enum languages)) None
        & info [ "l"; "lang"; "language" ] ~docv:"STRING" ~doc)
    in
    let in_paths =
      let doc = "paths of input JSON file representing Imandra IML values" in
      Arg.(value (pos_all string [] (info [] ~doc)))
    in
    let out_path =
      let doc = "path of output file (if absent, stdout)" in
      Arg.(
        value
        & opt (some string) None
        & info [ "o"; "output" ] ~docv:"PATH" ~doc)
    in
    Cmd.v (Cmd.info "imltk" ~doc) Term.(const main $ lang $ in_paths $ out_path)
  in
  Cmd.eval ~catch:false cmd
