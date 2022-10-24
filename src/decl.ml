(** Declarations  *)

type t = {
  loc: Loc.t;
  view: view;
}
[@@printer fun out self -> pp_view out self.view]

and view =
  | Ty of {
      recursive: bool;
      tys: Type.def list;
    }
  | Fun of {
      recursive: bool;
      fs: Term.fun_decl list;
    }
  | Module_alias of Uid.t * Uid.t
  | Module of {
      name: Uid.t;
      items: t list;
    }
[@@deriving show { with_path = false }, yojson]

type list_of = t list [@@deriving show, yojson]

open struct
  let mk_ ~loc view : t = { loc; view }
end

let ty ~loc ~recursive defs : t = mk_ ~loc @@ Ty { tys = defs; recursive }
let fun_ ~loc ~recursive defs : t = mk_ ~loc @@ Fun { recursive; fs = defs }

let ty_defs_of_decls (decls : t list) : Type.Defs.t =
  decls
  |> CCList.flat_map (fun d ->
         match d.view with
         | Ty { tys = defs; _ } -> defs
         | _ -> [])
  |> List.fold_left (fun defs d -> Type.Defs.add d defs) Type.Defs.empty
