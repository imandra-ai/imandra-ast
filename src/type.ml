(* Imandra/Pure type definitions *)

open Common_
module Var = Uid

type var = Var.t [@@deriving yojson, show, eq, ord]

type 't adt_row = { c: Uid.t; labels: Uid.t list option; args: 't list }
[@@deriving yojson, eq, ord, show { with_path = false }]
(** List of constructors for an algebraic type *)

type 't rec_row = { f: Uid.t; ty: 't }
[@@deriving yojson, eq, ord, show { with_path = false }]
(** List of record fields *)

(** Definition of a named type *)
type 't decl =
  | Algebraic of 't adt_row list
  | Record of 't rec_row list
  | Alias of { target: 't }
  | Skolem
  | Builtin of string
  | Other
[@@deriving yojson, eq, show { with_path = false }]

(** A general type expression for Imandra *)
type +'t view =
  | Var of var
  | Arrow of string * 't * 't
  | Tuple of 't list
  | Constr of Uid.t * 't list
[@@deriving yojson, eq, ord, map, iter, show { with_path = false }]

type t = { view: t view } [@@deriving yojson, eq, ord] [@@unboxed]

let[@inline] view (self : t) = self.view

(* Print a type_expr *)
let pp_as_ocaml_with ?(atomic = true) pp_id out ty : unit =
  let module Fmt = Fmt in
  let pp_lbl out = function
    | "" -> ()
    | lbl -> Format.fprintf out "%s:" lbl
  in
  let rec aux out (ty : t) =
    match ty.view with
    | Var v -> pp_id out v
    | Tuple xs ->
      Fmt.fprintf out "(@[%a@])" Fmt.(list ~sep:(return " *@ ") aux) xs
    | Constr (c, []) -> pp_id out c
    | Constr (c, [ x ]) -> Fmt.fprintf out "%a %a" aux_atomic x pp_id c
    | Constr (c, xs) ->
      Fmt.fprintf out "@[(@[%a@])@ %a@]"
        Fmt.(list ~sep:(return ",@ ") aux)
        xs pp_id c
    | Arrow (lbl, t, t') ->
      Fmt.fprintf out "@[%a%a ->@ %a@]" pp_lbl lbl aux_atomic t aux t'
  and aux_atomic out ty =
    match ty.view with
    | Arrow _ -> Fmt.fprintf out "(@[%a@])" aux ty
    | _ -> aux out ty
  in
  if atomic then
    aux_atomic out ty
  else
    aux out ty

let pp_as_ocaml = pp_as_ocaml_with ~atomic:false Uid.pp
let pp = pp_as_ocaml
let show = Fmt.to_string pp

type def = {
  params: var list;
  decl: t decl;
  name: Uid.t;
  name_loc: Loc.t;
  loc: Loc.t;
  codegen_tags: (string * string) list;
}
[@@deriving yojson, eq, show { with_path = false }]

let pp_def_name out (def : def) =
  Format.fprintf out "<@[type.t: %a@]>" Uid.pp def.name

module Defs = struct
  module M = Uid.Map

  type t = { defs: def M.t } [@@unboxed]

  let add (d : def) self : t = { defs = M.add d.name d self.defs }
  let empty : t = { defs = M.empty }
  let find (self : t) (n : Uid.t) : _ option = M.find_opt n self.defs

  let find_exn self n =
    match find self n with
    | Some d -> d
    | None -> Error.errorf "Cannot find type definition for `%a`" Uid.pp n

  let pp out (self : t) =
    Fmt.fprintf out "(@[defs@ [@[%a@]]@])"
      (Util.pp_iter ~sep:"," pp_def_name)
      (M.values self.defs)

  let show = Fmt.to_string pp
  let keys self = M.to_seq self.defs |> Seq.map fst |> CCSeq.to_rev_list
end

let mk_def ?(codegen_tags = []) ~loc ~name_loc ~name ~params decl : def =
  let def = { loc; name_loc; name; params; decl; codegen_tags } in
  def

let iter_def_uids def k =
  k def.name;
  match def.decl with
  | Algebraic l ->
    List.iter
      (fun c ->
        k c.c;
        CCOption.iter (List.iter k) c.labels)
      l
  | Record l -> List.iter (fun r -> k r.f) l
  | Alias _ | Builtin _ | Skolem | Other -> ()

let fold_def_uids f acc def =
  let acc = ref acc in
  iter_def_uids def (fun x -> acc := f !acc x);
  !acc

let free_vars_add =
  let rec aux acc t =
    match view t with
    | Var v -> Var.Set.add v acc
    | Arrow (_, a, b) -> aux (aux acc a) b
    | Tuple l | Constr (_, l) -> List.fold_left aux acc l
  in
  aux

let free_vars (t : t) : Var.Set.t = free_vars_add Var.Set.empty t

let rec is_ground ty =
  match view ty with
  | Var _ -> false
  | Arrow (_, a, b) -> is_ground a && is_ground b
  | Tuple l | Constr (_, l) -> List.for_all is_ground l

let is_poly ty = not (is_ground ty)

let is_ground_def d =
  match d.params with
  | [] -> true
  | _ -> false

let name def = def.name

let cstors_of_def def : _ list =
  match def.decl with
  | Algebraic rows -> List.map (fun r -> r.c) rows
  | _ -> []

let find_cstor_exn (def : def) (c : Uid.t) : _ adt_row =
  match def.decl with
  | Algebraic rows -> List.find (fun row -> Uid.equal c row.c) rows
  | _ -> raise Not_found

let find_cstor_by_name_exn (def : def) (name : string) : _ adt_row =
  match def.decl with
  | Algebraic rows -> List.find (fun row -> Uid.name row.c = name) rows
  | _ -> raise Not_found

let is_inline_record_cstor (d : def) (c : Uid.t) : bool =
  match d.decl with
  | Algebraic rows ->
    List.exists
      (function
        | { c = c'; labels = Some _; _ } -> Uid.equal c c'
        | _ -> false)
      rows
  | _ -> false

let mk_other_def ~loc (name : Uid.t) : def =
  { name; loc; name_loc = loc; decl = Other; params = []; codegen_tags = [] }

let mk_skolem_def ~loc c : def =
  {
    name = c;
    decl = Skolem;
    params = [];
    loc;
    name_loc = loc;
    codegen_tags = [];
  }

let[@inline] mk_ (view : _ view) : t = { view }
let mk_var v : t = mk_ @@ Var v
let mk_constr c args : t = mk_ @@ Constr (c, args)
let mk_constr0 c : t = mk_constr c []
let mk_arrow ?(lbl = "") a ret = mk_ @@ Arrow (lbl, a, ret)

let as_arrow ty =
  match view ty with
  | Arrow (s, a, b) -> Some (s, a, b)
  | _ -> None

let as_arrow2 ty =
  match view ty with
  | Arrow (_, a, { view = Arrow (_, b, c); _ }) -> Some (a, b, c)
  | _ -> None

let mk_tuple l : t =
  assert (l <> []);
  mk_ @@ Tuple l

let rec mk_arrow_l args ret : t =
  match args with
  | [] -> ret
  | ty1 :: args' -> mk_ @@ Arrow ("", ty1, mk_arrow_l args' ret)

let mk_skolem c : t = mk_constr c []

let rec compare (a : t) (b : t) : int =
  if a == b then
    0
  else
    compare_view compare (view a) (view b)

let rec equal (a : t) (b : t) : bool =
  a == b || equal_view equal (view a) (view b)

let rec equal_with ~chase (a : t) (b : t) : bool =
  a == b
  ||
  let a = chase a in
  let b = chase b in
  equal_view (equal_with ~chase) (view a) (view b)

let hash (t : t) : int =
  let module H = CCHash in
  let rec aux (a : t) =
    match view a with
    | Var v -> H.combine2 10 (Var.hash v)
    | Arrow (_, a, b) -> H.combine3 20 (aux a) (aux b)
    | Tuple l -> H.combine2 30 (H.list aux l)
    | Constr (p, l) -> H.combine3 40 (Uid.hash p) (H.list aux l)
  in
  aux t

let iter_shallow ty f =
  match ty.view with
  | Var _ -> ()
  | _ -> iter_view f ty.view

let map_shallow ty ~f : t =
  match ty.view with
  | Var _ -> ty
  | Arrow _ | Tuple _ | Constr _ ->
    let view' = map_view f ty.view in
    if equal_view ( == ) ty.view view' then
      ty
    else
      mk_ view'

(* Substitute a type for a type variable *)

module Subst = struct
  type ty = t
  type nonrec t = t Var.Map.t

  let empty = Var.Map.empty

  let pp out (s : t) =
    let pp_binding out (x, y) =
      Format.fprintf out "(@[%s @<1>→ %a@])" (Var.show x) pp y
    in
    Fmt.(within "{" "}" @@ hovbox @@ list ~sep:(return "@ :: ") pp_binding)
      out (Var.Map.bindings s)

  let show = Fmt.to_string pp

  let find (v : var) (subst : t) : _ option =
    try Some (Var.Map.find v subst) with Not_found -> None

  let bind (v : var) (t : ty) (subst : t) : t =
    assert (not (Var.Map.mem v subst));
    Var.Map.add v t subst

  let rec apply_rec_ ~rec_ (ty : ty) (subst : t) : ty =
    match ty.view with
    | Var v ->
      (match find v subst with
      | None -> ty
      | Some ty' when rec_ -> apply_rec_ ~rec_ ty' subst
      | Some ty' -> ty')
    | _ -> map_shallow ty ~f:(fun u -> apply_rec_ ~rec_ u subst)

  let apply ?(rec_ = true) ty subst =
    if Var.Map.is_empty subst then
      ty
    else
      apply_rec_ ~rec_ ty subst

  let apply' ?rec_ subst ty = apply ?rec_ ty subst
end

(** {2 Type unification} *)

(* evaluate [(Λparams. target) ts] *)
let expand_alias (ty : t) (target : t) (params : var list) (ts : t list) : t =
  if List.length params <> List.length ts then
    Error.errorf "type %a is partially applied (needs %d args, got %d)" pp ty
      (List.length params) (List.length ts);
  let subst =
    List.fold_left2
      (fun subst v arg -> Var.Map.add v arg subst)
      Var.Map.empty params ts
  in
  Subst.apply ~rec_:false target subst

(* dereference [t] in [env], as long as [t] is a bound variable
   or an alias. *)
let[@unroll 1] rec chase_rec (defs : Defs.t) env (ty : t) : t =
  let[@inline] recurse ty = chase_rec defs env ty in
  match view ty with
  | Var v ->
    (match Subst.find v env with
    | None -> ty
    | Some ty' -> recurse ty')
  | Constr (c, ts) ->
    (match Defs.find defs c with
    | Some { decl = Alias { target }; params; _ } ->
      (* alias expansion *)
      let u = expand_alias ty target params ts in
      recurse u
    | _ -> ty)
  | _ -> ty

let[@inline] chase_shallow_ defs ty : t = chase_rec defs Subst.empty ty

(* Obtain input/output types for `essentially first-order' functions *)

let rec returns defs ty : t =
  let ty = chase_shallow_ defs ty in
  match view ty with
  | Arrow (_, _, ret) -> returns defs ret
  | _ -> ty

let rec drop_n_args defs n ty : t =
  if n <= 0 then
    ty
  else (
    let ty = chase_shallow_ defs ty in
    match view ty with
    | Arrow (_, _, ret) -> drop_n_args defs (n - 1) ret
    | _ -> ty
  )

let rec in_out_types defs (ty : t) : _ * t =
  let ty = chase_shallow_ defs ty in
  match view ty with
  | Arrow (_, a, b) ->
    let ins, out = in_out_types defs b in
    a :: ins, out
  | _ -> [], ty

let rec in_out_types_no_chase ty =
  match view ty with
  | Arrow (_, a, b) ->
    let ins, out = in_out_types_no_chase b in
    a :: ins, out
  | _ -> [], ty

let rec n_args defs ty =
  let ty = chase_shallow_ defs ty in
  match view ty with
  | Arrow (_, _, b) -> 1 + n_args defs b
  | _ -> 0

let is_arrow defs ty =
  let ty = chase_shallow_ defs ty in
  match view ty with
  | Arrow _ -> true
  | _ -> false

let is_param defs ty =
  is_poly ty
  ||
  let ins, _ = in_out_types defs ty in
  List.exists (is_arrow defs) ins

let chase defs ?(subst = Subst.empty) t = chase_rec defs subst t

let chase_deep defs ?(subst = Subst.empty) t =
  let rec aux t =
    let t = chase_rec defs subst t in
    match view t with
    | Var _ -> t
    | _ -> map_shallow t ~f:aux
  in
  aux t

let equal_chase_deep defs a b : bool =
  let[@inline] chase ty = chase_rec defs Subst.empty ty in
  equal_with ~chase a b

let same_base_type defs x y =
  let x = chase_rec defs Subst.empty x in
  let y = chase_rec defs Subst.empty y in
  match view x, view y with
  | Constr (c, _), Constr (c', _) -> Uid.equal c c'
  | _ -> false

let head_def_of_ty (defs : Defs.t) ty : def =
  let ty = chase defs ty in
  match view ty with
  | Constr (c, _) -> Defs.find_exn defs c
  | _ -> Error.errorf "(head_def_of_ty) type %a is not a defined type" pp ty

(* Basic predicates for built-in types *)

module As_key = struct
  type nonrec t = t

  let equal = equal
  let hash = hash
  let compare = compare
end

module Tbl = CCHashtbl.Make (As_key)
module Map = CCMap.Make (As_key)

let bool = mk_constr Uid.Builtins.bool []
