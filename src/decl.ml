(** Declarations  *)

type t = { loc: Loc.t; view: view }

and view = Ty of Type.def list | Fun of Term.fun_decl list
[@@deriving show, yojson]
