(** {1 Syntactic Terms} *)

open Common_

type rec_flag = Recursive | Nonrecursive
[@@deriving yojson, eq, ord, show { with_path = false }]

type apply_label = Nolabel | Label of string | Optional of string
[@@deriving yojson, eq, ord, show { with_path = false }]

type const =
  | Const_nativeint of (nativeint[@encoding `string])
  | Const_int32 of int32
  | Const_int64 of (int64[@encoding `string])
  | Const_float of float
  | Const_char of char
  | Const_string of string
  | Const_z of (Z.t[@ser Util_cbpack_.ser_z] [@deser Util_cbpack_.deser_z])
  | Const_q of (Q.t[@ser Util_cbpack_.ser_q] [@deser Util_cbpack_.deser_q])
  | Const_real_approx of string
[@@deriving yojson, eq, ord, show { with_path = false }]


type 'a with_loc = { view: 'a; loc: Loc.t }
[@@deriving yojson, eq, ord, show { with_path = false }]

let[@inline] view self = self.view

type pattern = pattern_view with_loc

and pattern_view =
  | P_or of pattern * pattern
  | P_var of Var.t
  | P_construct of { c: Uid.t; ty: Type.t; args: pattern list; lbls: Uid.t list option }
  | P_tuple of Type.t * pattern list
  | P_record of Type.t * (Uid.t * pattern) list
  | P_any of Type.t
  | P_alias of Var.t * pattern (* pat as Var.t *)
  | P_true
  | P_false
  | P_const of const * Type.t
[@@deriving cbpack]

(* branch in a pattern matching *)
type 't vb = { pat: pattern; when_: 't option; (* side condition *) expr: 't }
[@@deriving cbpack]

type t = t_view with_loc

and t_view =
  | Const of const * Type.t
  | If of t * t * t
  | Let of rec_flag * binding list * t
  | Apply of Type.t * t * apply_arg list
  | Fun of Type.t * apply_label * Var.t * t
  | Ident of Var.t
  | Construct of { c: Uid.t; Type.t: Type.t; args: t list; lbls: Uid.t list option }
  | Tuple of Type.t * t list
  | Field of { data_ty: Type.t; Type.t: Type.t; f: Uid.t; t: t }
  | Record of Type.t * (Uid.t * t) list * t option
  | Match of { loc: Iloc.t option; Type.t: Type.t; lhs: t; bs: t vb list }
  | Let_match of { loc: Iloc.t option; flg: rec_flag; bs: t vb list; body: t }
  | True
  | False
  | As of t * Type.t
  | Trigger of { t: t; tag: as_trigger }
(* annotated trigger *)

and apply_arg = apply_label * t

(* simple variable binding *)
and binding = Var.t * t [@@deriving cbpack]

let rec Type.t (t : t) =
  match t.view with
  | Const (_, Type.t)
  | Apply (Type.t, _, _)
  | Fun (Type.t, _, _, _)
  | Construct { Type.t; _ }
  | Tuple (Type.t, _)
  | Field { Type.t; _ }
  | Record (Type.t, _, _)
  | Match { Type.t; _ }
  | As (_, Type.t) ->
    Type.t
  | If (_, b, _c) ->
    let ty_b = Type.t b in
    ty_b
  | Ident v -> A.var_ty v
  | Let_match { body; _ } -> Type.t body
  | Let (_, _, body) -> Type.t body
  | Trigger { t; _ } -> Type.t t
  | True | False -> A.ty_bool ()

(* TODO: make [loc] non optional *)
let[@inline] mk ~loc view : t = { view; loc }
let[@inline] mk_pat ~loc view : pattern = { view; loc }
let Var.t ~loc v : t = mk ~loc @@ Ident v
let const ~loc ~Type.t c : t = mk ~loc @@ Const (c, Type.t)
let as_ ~loc t Type.t : t = mk ~loc @@ As (t, Type.t)
let if_ ~loc a b c : t = mk ~loc @@ If (a, b, c)
let let_ ~loc ~flg bs body : t = mk ~loc @@ Let (flg, bs, body)
let tuple ~loc ~Type.t l : t = mk ~loc @@ Tuple (Type.t, l)

let apply ~loc ~Type.t f l : t =
  match f.view, l with
  | _, [] -> f
  | Apply (_, f1, l1), _ -> mk ~loc @@ Apply (Type.t, f1, l1 @ l)
  | _ -> mk ~loc @@ Apply (Type.t, f, l)

let rec equal (a : t) (b : t) =
  match a.view, b.view with
  | True, True | False, False -> true
  | Const (c1, ty1), Const (c2, ty2) -> equal_const c1 c2 && equal_ty ty1 ty2
  | If (a1, b1, c1), If (a2, b2, c2) ->
    equal a1 a2 && equal b1 b2 && equal c1 c2
  | Let (rec1, bs1, body1), Let (rec2, bs2, body2) ->
    rec1 = rec2
    && CCList.equal (CCPair.equal equal_var equal) bs1 bs2
    && equal body1 body2
  | Apply (ty1, f1, args1), Apply (ty2, f2, args2) ->
    A.equal_ty ty1 ty2 && equal f1 f2 && CCList.equal equal_arg args1 args2
  | Ident v1, Ident v2 -> equal_var v1 v2
  | ( Construct { c = c1; Type.t = ty1; args = args1; lbls = lbls1 },
      Construct { c = c2; Type.t = ty2; args = args2; lbls = lbls2 } ) ->
    equal_id c1 c2 && equal_ty ty1 ty2
    && CCList.equal equal args1 args2
    && CCOption.equal (CCList.equal equal_id) lbls1 lbls2
  | Tuple (ty1, args1), Tuple (ty2, args2) ->
    equal_ty ty1 ty2 && CCList.equal equal args1 args2
  | ( Field { data_ty = ty1; f = u1; t = target1; _ },
      Field { data_ty = ty2; f = u2; t = target2; _ } ) ->
    equal_ty ty1 ty2 && equal_id u1 u2 && equal target1 target2
  | Record (ty1, kvs1, with1), Record (ty2, kvs2, with2) ->
    equal_ty ty1 ty2
    && CCList.equal (CCPair.equal equal_id equal) kvs1 kvs2
    && CCOption.equal equal with1 with2
  | Fun (_, lbl1, v1, body1), Fun (_, lbl2, v2, body2) ->
    lbl1 = lbl2 && equal_var v1 v2 && equal body1 body2
  | ( Match { Type.t = ty1; lhs = lhs1; bs = bs1; _ },
      Match { Type.t = ty2; lhs = lhs2; bs = bs2; _ } ) ->
    equal_ty ty1 ty2 && equal lhs1 lhs2 && CCList.equal vb_equal bs1 bs2
  | ( Let_match { flg = flg1; bs = bs1; body = body1; _ },
      Let_match { flg = flg2; bs = bs2; body = body2; _ } ) ->
    flg1 = flg2 && CCList.equal vb_equal bs1 bs2 && equal body1 body2
  | As (t1, ty1), As (t2, ty2) -> equal t1 t2 && equal_ty ty1 ty2
  | Trigger r1, Trigger r2 -> equal r1.t r2.t && As_trigger.equal r1.tag r2.tag
  | ( ( True | False | Const _ | If _ | Let _ | Apply _ | Ident _ | Construct _
      | Tuple _ | Field _ | Record _ | Fun _ | Match _ | Let_match _ | As _
      | Trigger _ ),
      _ ) ->
    false

and equal_arg a b = fst a = fst b && equal (snd a) (snd b)

and vb_equal { pat = pat1; when_ = when1; expr = expr1 }
    { pat = pat2; when_ = when2; expr = expr2 } =
  pat_equal pat1 pat2 && CCOption.equal equal when1 when2 && equal expr1 expr2

and pat_equal (p1 : pattern) (p2 : pattern) =
  match p1.view, p2.view with
  | P_true, P_true | P_false, P_false -> true
  | P_or (p11, p12), P_or (p21, p22) -> pat_equal p11 p21 && pat_equal p12 p22
  | P_var v1, P_var v2 -> equal_var v1 v2
  | ( P_construct { c = c1; Type.t = ty1; args = args1; lbls = lbls1; _ },
      P_construct { c = c2; Type.t = ty2; args = args2; lbls = lbls2; _ } ) ->
    equal_id c1 c2 && equal_ty ty1 ty2
    && CCList.equal pat_equal args1 args2
    && CCOption.equal (CCList.equal equal_id) lbls1 lbls2
  | P_tuple (ty1, pts1), P_tuple (ty2, pts2) ->
    equal_ty ty1 ty2 && CCList.equal pat_equal pts1 pts2
  | P_record (ty1, pts1), P_record (ty2, pts2) ->
    equal_ty ty1 ty2 && CCList.equal (CCPair.equal equal_id pat_equal) pts1 pts2
  | P_any ty1, P_any ty2 -> equal_ty ty1 ty2
  | P_alias (v1, pat1), P_alias (v2, pat2) ->
    equal_var v1 v2 && pat_equal pat1 pat2
  | P_const (c1, _), P_const (c2, _) -> equal_const c1 c2
  | ( ( P_true | P_false | P_or _ | P_var _ | P_construct _ | P_tuple _
      | P_record _ | P_any _ | P_alias _ | P_const _ ),
      _ ) ->
    false

let rec pattern_ty (p : pattern) =
  match p.view with
  | P_var v | P_alias (v, _) -> A.var_ty v
  | P_true | P_false -> A.ty_bool ()
  | P_or (a, _) -> pattern_ty a
  | P_any Type.t
  | P_const (_, Type.t)
  | P_construct { Type.t; _ }
  | P_tuple (Type.t, _)
  | P_record (Type.t, _) ->
    Type.t
