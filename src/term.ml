(** {1 Syntactic Terms} *)

open Common_
module J = Yojson.Safe

type rec_flag =
  | Recursive
  | Nonrecursive
[@@deriving yojson, eq, ord, show { with_path = false }]

type apply_label =
  | Nolabel
  | Label of string
  | Optional of string
[@@deriving yojson, eq, ord, show { with_path = false }]

type const =
  | Const_nativeint of (nativeint[@encoding `string])
  | Const_int32 of int32
  | Const_int64 of (int64[@encoding `string])
  | Const_float of float
  | Const_char of char
  | Const_string of string
  | Const_z of
      (Z.t
      [@printer Z.pp_print]
      [@to_yojson fun n -> `String (Z.to_string n)]
      [@of_yojson
        fun s ->
          try Ok (J.Util.to_string s |> Z.of_string)
          with _ -> Error "invalid Z"])
  | Const_q of
      (Q.t
      [@printer Q.pp_print]
      [@to_yojson fun n -> `String (Q.to_string n)]
      [@of_yojson
        fun s ->
          try Ok (J.Util.to_string s |> Q.of_string)
          with _ -> Error "invalid Q"])
  | Const_real_approx of string
[@@deriving yojson, eq, ord, show { with_path = false }]

type 'a with_loc = {
  view: 'a;
  loc: Loc.t;
}
[@@deriving yojson, eq, ord]

let pp_with_loc ppx out { view; loc = _ } = ppx out view
let show_with_loc ppx = Fmt.to_string (pp_with_loc ppx)
let[@inline] view self = self.view

type pattern = pattern_view with_loc

and pattern_view =
  | P_or of pattern * pattern
  | P_var of Var.t
  | P_construct of {
      c: Uid.t;
      ty: Type.t;
      args: pattern list;
      lbls: Uid.t list option;
    }
  | P_tuple of Type.t * pattern list
  | P_record of Type.t * (Uid.t * pattern) list
  | P_any of Type.t
  | P_alias of Var.t * pattern (* pat as Var.t *)
  | P_true
  | P_false
  | P_const of const * Type.t
[@@deriving yojson, eq, ord, show { with_path = false }]

(* branch in a pattern matching *)
type 't vb = {
  pat: pattern;
  when_: 't option;
  expr: 't;
}
[@@deriving yojson, eq, ord, show { with_path = false }]

type t = t_view with_loc

and t_view =
  | Const of const * Type.t
  | If of t * t * t
  | Let of rec_flag * binding list * t
  | Apply of Type.t * t * apply_arg list
  | Fun of Type.t * apply_label * Var.t * t
  | Ident of Var.t
  | Construct of {
      c: Uid.t;
      ty: Type.t;
      args: t list;
      lbls: Uid.t list option;
    }
  | Tuple of Type.t * t list
  | Field of {
      data_ty: Type.t;
      ty: Type.t;
      f: Uid.t;
      t: t;
    }
  | Record of Type.t * (Uid.t * t) list * t option
  | Match of {
      loc: Loc.t option;
      ty: Type.t;
      lhs: t;
      bs: t vb list;
    }
  | Let_match of {
      loc: Loc.t option;
      flg: rec_flag;
      bs: t vb list;
      body: t;
    }
  | True
  | False
  | As of t * Type.t

and apply_arg = apply_label * t

(* simple variable binding *)
and binding = Var.t * t [@@deriving yojson, eq, ord, show { with_path = false }]

let rec ty (t : t) =
  match t.view with
  | Const (_, ty)
  | Apply (ty, _, _)
  | Fun (ty, _, _, _)
  | Construct { ty; _ }
  | Tuple (ty, _)
  | Field { ty; _ }
  | Record (ty, _, _)
  | Match { ty; _ }
  | As (_, ty) ->
    ty
  | If (_, b, _c) ->
    let ty_b = ty b in
    ty_b
  | Ident v -> Var.ty v
  | Let_match { body; _ } -> ty body
  | Let (_, _, body) -> ty body
  | True | False -> Type.bool

(* patterns *)

let[@inline] mk_pat ~loc view : pattern = { view; loc }
let p_or ~loc a b : pattern = mk_pat ~loc @@ P_or (a, b)
let p_const ~loc ~ty c : pattern = mk_pat ~loc @@ P_const (c, ty)
let p_var ~loc v : pattern = mk_pat ~loc @@ P_var v
let p_any ~loc ty : pattern = mk_pat ~loc @@ P_any ty
let p_tuple ~loc ~ty l : pattern = mk_pat ~loc @@ P_tuple (ty, l)
let p_alias ~loc v p : pattern = mk_pat ~loc @@ P_alias (v, p)
let p_record ~loc ~ty fs : pattern = mk_pat ~loc @@ P_record (ty, fs)

let p_construct ~loc ~c ~ty ?lbls args : pattern =
  mk_pat ~loc @@ P_construct { c; ty; lbls; args }

let p_bool ~loc b : pattern =
  mk_pat ~loc
  @@
  if b then
    P_true
  else
    P_false

(* terms *)

let[@inline] mk ~loc view : t = { view; loc }
let var ~loc v : t = mk ~loc @@ Ident v
let const ~loc ~ty c : t = mk ~loc @@ Const (c, ty)
let as_ ~loc t ty : t = mk ~loc @@ As (t, ty)
let if_ ~loc a b c : t = mk ~loc @@ If (a, b, c)
let let_ ~loc ~flg bs body : t = mk ~loc @@ Let (flg, bs, body)
let record ~loc ~ty ?rest fields = mk ~loc @@ Record (ty, fields, rest)
let field ~loc ~data_ty ~ty f t : t = mk ~loc @@ Field { ty; data_ty; f; t }
let fun_ ~loc ~ty lbl v bod : t = mk ~loc @@ Fun (ty, lbl, v, bod)
let tuple ~loc ~ty l : t = mk ~loc @@ Tuple (ty, l)

let apply ~loc ~ty f l : t =
  match (f.view, l) with
  | _, [] -> f
  | Apply (_, f1, l1), _ -> mk ~loc @@ Apply (ty, f1, l1 @ l)
  | _ -> mk ~loc @@ Apply (ty, f, l)

let bool ~loc b : t =
  mk ~loc
    (if b then
      True
    else
      False)

let vb ?when_ pat expr : t vb = { when_; pat; expr }

let construct ~loc ~ty ?lbls ~c args : t =
  mk ~loc @@ Construct { c; args; ty; lbls }

let match_ ~loc ~ty lhs bs : t =
  mk ~loc @@ Match { loc = Some loc; ty; lhs; bs }

let let_match_ ~loc ~flg bs body : t =
  mk ~loc @@ Let_match { flg; loc = Some loc; bs; body }

let rec equal (a : t) (b : t) =
  match (a.view, b.view) with
  | True, True | False, False -> true
  | Const (c1, ty1), Const (c2, ty2) -> equal_const c1 c2 && Type.equal ty1 ty2
  | If (a1, b1, c1), If (a2, b2, c2) ->
    equal a1 a2 && equal b1 b2 && equal c1 c2
  | Let (rec1, bs1, body1), Let (rec2, bs2, body2) ->
    rec1 = rec2
    && CCList.equal (CCPair.equal Var.equal equal) bs1 bs2
    && equal body1 body2
  | Apply (ty1, f1, args1), Apply (ty2, f2, args2) ->
    Type.equal ty1 ty2 && equal f1 f2 && CCList.equal equal_arg args1 args2
  | Ident v1, Ident v2 -> Var.equal v1 v2
  | ( Construct { c = c1; ty = ty1; args = args1; lbls = lbls1 },
      Construct { c = c2; ty = ty2; args = args2; lbls = lbls2 } ) ->
    Uid.equal c1 c2 && Type.equal ty1 ty2
    && CCList.equal equal args1 args2
    && CCOption.equal (CCList.equal Uid.equal) lbls1 lbls2
  | Tuple (ty1, args1), Tuple (ty2, args2) ->
    Type.equal ty1 ty2 && CCList.equal equal args1 args2
  | ( Field { data_ty = ty1; f = u1; t = target1; _ },
      Field { data_ty = ty2; f = u2; t = target2; _ } ) ->
    Type.equal ty1 ty2 && Uid.equal u1 u2 && equal target1 target2
  | Record (ty1, kvs1, with1), Record (ty2, kvs2, with2) ->
    Type.equal ty1 ty2
    && CCList.equal (CCPair.equal Uid.equal equal) kvs1 kvs2
    && CCOption.equal equal with1 with2
  | Fun (_, lbl1, v1, body1), Fun (_, lbl2, v2, body2) ->
    lbl1 = lbl2 && Var.equal v1 v2 && equal body1 body2
  | ( Match { ty = ty1; lhs = lhs1; bs = bs1; _ },
      Match { ty = ty2; lhs = lhs2; bs = bs2; _ } ) ->
    Type.equal ty1 ty2 && equal lhs1 lhs2 && CCList.equal vb_equal bs1 bs2
  | ( Let_match { flg = flg1; bs = bs1; body = body1; _ },
      Let_match { flg = flg2; bs = bs2; body = body2; _ } ) ->
    flg1 = flg2 && CCList.equal vb_equal bs1 bs2 && equal body1 body2
  | As (t1, ty1), As (t2, ty2) -> equal t1 t2 && Type.equal ty1 ty2
  | ( ( True | False | Const _ | If _ | Let _ | Apply _ | Ident _ | Construct _
      | Tuple _ | Field _ | Record _ | Fun _ | Match _ | Let_match _ | As _ ),
      _ ) ->
    false

and equal_arg a b = fst a = fst b && equal (snd a) (snd b)

and vb_equal { pat = pat1; when_ = when1; expr = expr1 }
    { pat = pat2; when_ = when2; expr = expr2 } =
  pat_equal pat1 pat2 && CCOption.equal equal when1 when2 && equal expr1 expr2

and pat_equal (p1 : pattern) (p2 : pattern) =
  match (p1.view, p2.view) with
  | P_true, P_true | P_false, P_false -> true
  | P_or (p11, p12), P_or (p21, p22) -> pat_equal p11 p21 && pat_equal p12 p22
  | P_var v1, P_var v2 -> Var.equal v1 v2
  | ( P_construct { c = c1; ty = ty1; args = args1; lbls = lbls1; _ },
      P_construct { c = c2; ty = ty2; args = args2; lbls = lbls2; _ } ) ->
    Uid.equal c1 c2 && Type.equal ty1 ty2
    && CCList.equal pat_equal args1 args2
    && CCOption.equal (CCList.equal Uid.equal) lbls1 lbls2
  | P_tuple (ty1, pts1), P_tuple (ty2, pts2) ->
    Type.equal ty1 ty2 && CCList.equal pat_equal pts1 pts2
  | P_record (ty1, pts1), P_record (ty2, pts2) ->
    Type.equal ty1 ty2
    && CCList.equal (CCPair.equal Uid.equal pat_equal) pts1 pts2
  | P_any ty1, P_any ty2 -> Type.equal ty1 ty2
  | P_alias (v1, pat1), P_alias (v2, pat2) ->
    Var.equal v1 v2 && pat_equal pat1 pat2
  | P_const (c1, _), P_const (c2, _) -> equal_const c1 c2
  | ( ( P_true | P_false | P_or _ | P_var _ | P_construct _ | P_tuple _
      | P_record _ | P_any _ | P_alias _ | P_const _ ),
      _ ) ->
    false

let rec pattern_ty (p : pattern) =
  match p.view with
  | P_var v | P_alias (v, _) -> Var.ty v
  | P_true | P_false -> Type.bool
  | P_or (a, _) -> pattern_ty a
  | P_any ty
  | P_const (_, ty)
  | P_construct { ty; _ }
  | P_tuple (ty, _)
  | P_record (ty, _) ->
    ty

type fun_decl = {
  name: Uid.t;
  pat: pattern;
  body: t;
  loc: Loc.t;
  name_loc: Loc.t;  (** loc of [name] *)
  codegen_tags: (string * string) list;
}
[@@deriving yojson, eq, ord, show { with_path = false }]
(** Function declaration *)

let mk_fun_decl ?(codegen_tags = []) ?name_loc ~loc ~name ~pat ~body () :
    fun_decl =
  let name_loc = Option.value ~default:loc name_loc in
  { name; pat; body; loc; name_loc; codegen_tags }
