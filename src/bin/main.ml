module Fmt = CCFormat
module A = Imandra_ast

let main () =
  let out = ref "" in
  let cg_rust = ref false in
  let files = ref [] in
  let options =
    [
      ("-o", Arg.Set_string out, " output file");
      ("--rust", Arg.Set cg_rust, " generate rust code");
    ]
    |> Arg.align
  in

  Arg.parse options (fun f -> files := f :: !files) "imltk [option]* [file]+";
  let files = List.rev !files in

  (* parse input files *)
  let decls =
    CCList.flat_map
      (fun f ->
        let l =
          try Yojson.Safe.from_file f
          with e ->
            Fmt.eprintf "could not read %S@." f;
            exit 1
        in

        match A.Decl.list_of_of_yojson l with
        | Ok l -> l
        | Error err ->
          Fmt.eprintf "could not decode AST: %s@." err;
          exit 2)
      files
  in

  Fmt.eprintf "parsed %d declarations@." (List.length decls);

  if !cg_rust then (
    let code = Imandra_ast_cg_rust.codegen decls in
    if !out <> "" then
      CCIO.File.write_exn !out code
    else
      print_endline code
  );
  ()

let () = main ()
