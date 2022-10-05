open Common_

(** {1 Definitions} *)

type rec_flag =
  | Recursive
  | Nonrecursive
[@@deriving show, eq, ord, yojson]

type apply_label =
  | Nolabel
  | Label of string
  | Optional of string
[@@deriving show, eq, ord, yojson]

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
[@@deriving yojson, eq, ord, show]

type 'a with_loc = {
  view: 'a;
  loc: Loc.t;
}
[@@deriving show, yojson, eq, ord]

val view : 'a with_loc -> 'a

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
[@@deriving yojson, eq, ord, show]

(* branch in a pattern matching *)
type 't vb = {
  pat: pattern;
  when_: 't option;  (** side condition *)
  expr: 't;
}
[@@deriving eq, show, ord, yojson]

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
and binding = Var.t * t [@@deriving eq, show, ord, yojson]

(** {1 Patterns} *)

val pattern_ty : pattern -> Type.t
val mk_pat : loc:Loc.t -> pattern_view -> pattern
val p_or : loc:Loc.t -> pattern -> pattern -> pattern
val p_var : loc:Loc.t -> Var.t -> pattern
val p_tuple : loc:Loc.t -> ty:Type.t -> pattern list -> pattern
val p_record : loc:Loc.t -> ty:Type.t -> (Uid.t * pattern) list -> pattern
val p_any : loc:Loc.t -> Type.t -> pattern
val p_alias : loc:Loc.t -> Var.t -> pattern -> pattern
val p_bool : loc:Loc.t -> bool -> pattern
val p_const : loc:Loc.t -> ty:Type.t -> const -> pattern

val p_construct :
  loc:Loc.t ->
  c:Uid.t ->
  ty:Type.t ->
  ?lbls:Uid.t list ->
  pattern list ->
  pattern

(** {1 Terms} *)

val mk : loc:Loc.t -> t_view -> t
val bool : loc:Loc.t -> bool -> t
val ty : t -> Type.t
val var : loc:Loc.t -> Var.t -> t
val apply : loc:Loc.t -> ty:Type.t -> t -> apply_arg list -> t
val const : loc:Loc.t -> ty:Type.t -> const -> t
val field : loc:Loc.t -> data_ty:Type.t -> ty:Type.t -> Uid.t -> t -> t
val match_ : loc:Loc.t -> ty:Type.t -> t -> t vb list -> t
val let_match_ : loc:Loc.t -> flg:rec_flag -> t vb list -> t -> t
val fun_ : loc:Loc.t -> ty:Type.t -> apply_label -> Var.t -> t -> t
val record : loc:Loc.t -> ty:Type.t -> ?rest:t -> (Uid.t * t) list -> t
val let_ : loc:Loc.t -> flg:rec_flag -> binding list -> t -> t
val tuple : loc:Loc.t -> ty:Type.t -> t list -> t
val if_ : loc:Loc.t -> t -> t -> t -> t
val as_ : loc:Loc.t -> t -> Type.t -> t
val equal : t -> t -> bool
val vb : ?when_:t -> pattern -> t -> t vb

val construct :
  loc:Loc.t -> ty:Type.t -> ?lbls:Uid.t list -> c:Uid.t -> t list -> t

(** {1 Functions} *)

type fun_decl = private {
  name: Uid.t;
  params: Var.t list;
  body: t;
  loc: Loc.t;
  name_loc: Loc.t;
  codegen_tags: (string * string) list;
}
[@@deriving eq, show, ord, yojson]
(** Function declaration *)

val mk_fun_decl :
  ?codegen_tags:(string * string) list ->
  ?name_loc:Loc.t ->
  loc:Loc.t ->
  name:Uid.t ->
  params:Var.t list ->
  body:t ->
  unit ->
  fun_decl
