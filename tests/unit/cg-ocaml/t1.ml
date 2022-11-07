module Fmt = CCFormat
module A = Imandra_ast

let decls_json = CCIO.File.read_exn "./t1.json"

let decls =
  match A.Decl.list_of_of_yojson @@ Yojson.Safe.from_string decls_json with
  | Ok d -> d
  | Error e ->
    Fmt.eprintf "cannot decode decls: %s" e;
    assert false

let code = Imandra_ast_cg_ocaml.codegen decls
let () = Fmt.printf "ocaml code:@.```@.%s@.```@." code
