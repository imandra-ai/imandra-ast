(** Declarations  *)

type t = {
  loc: Loc.t;
  view: view;
}
[@@printer fun out self -> pp_view out self.view]

and view =
  | Ty of { tys: Type.def list }
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

let ty ~loc defs : t = mk_ ~loc @@ Ty { tys = defs }
let fun_ ~loc ~recursive defs : t = mk_ ~loc @@ Fun { recursive; fs = defs }

let ty_defs_of_decls (decls : t list) : Type.Defs.t =
  decls
  |> CCList.flat_map (fun d ->
         match d.view with
         | Ty { tys = defs; _ } -> defs
         | _ -> [])
  |> List.fold_left (fun defs d -> Type.Defs.add d defs) Type.Defs.empty

let rec defined_ids ?(init = Uid.Set.empty) (self : t) : Uid.Set.t =
  match self.view with
  | Ty { tys } ->
    List.fold_left
      (fun set (ty_d : Type.def) -> Type.defined_ids ~init:set ty_d)
      init tys
  | Fun { fs; _ } ->
    List.fold_left (fun set f -> Term.defined_ids ~init:set f) init fs
  | Module_alias (m, _) -> Uid.Set.add m init
  | Module { name; items } ->
    let set = Uid.Set.add name init in
    List.fold_left (fun set d -> defined_ids ~init:set d) set items

let defined_ids_of_decls (decls : t list) : Uid.Set.t =
  List.fold_left (fun set d -> defined_ids ~init:set d) Uid.Set.empty decls
