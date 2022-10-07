(** Declarations  *)

type t = {
  loc: Loc.t;
  view: view;
}
[@@printer fun out self -> pp_view out self.view]

and view =
  | Ty of Type.def list
  | Fun of {
      recursive: bool;
      fs: Term.fun_decl list;
    }
[@@deriving show { with_path = false }, yojson]

open struct
  let mk_ ~loc view : t = { loc; view }
end

let ty ~loc defs : t = mk_ ~loc @@ Ty defs
let fun_ ~loc ~recursive defs : t = mk_ ~loc @@ Fun { recursive; fs = defs }
