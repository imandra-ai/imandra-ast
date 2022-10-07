(** Declarations  *)

type t = {
  loc: Loc.t;
  view: view;
}

and view =
  | Ty of Type.def list
  | Fun of Term.fun_decl list
[@@deriving show, yojson]

open struct
  let mk_ ~loc view : t = { loc; view }
end

let ty ~loc defs : t = mk_ ~loc @@ Ty defs
let fun_ ~loc defs : t = mk_ ~loc @@ Fun defs
