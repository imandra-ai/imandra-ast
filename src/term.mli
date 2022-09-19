open Common_

type rec_flag = Recursive | Nonrecursive [@@deriving show, eq, ord, yojson]

type apply_label = Nolabel | Label of string | Optional of string
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
[@@deriving yojson, eq, ord, show { with_path = false }]

type 'a with_loc = { view: 'a; loc: Loc.t } [@@deriving show, yojson, eq,ord]

  val view : 'a with_loc -> 'a

type pattern = pattern_view with_loc

and pattern_view =
  | P_or of pattern * pattern
  | P_var of Var.t
  | P_construct of { c: Uid.t; Type.t: Type.t; args: pattern list; lbls: Uid.t list option }
  | P_tuple of Type.t * pattern list
  | P_record of Type.t * (Uid.t * pattern) list
  | P_any of Type.t
  | P_alias of Var.t * pattern (* pat as Var.t *)
  | P_true
  | P_false
  | P_const of const * Type.t
[@@deriving yojson]

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
and binding = Var.t * t

(* branch in a pattern matching *)
and 't vb = { pat: pattern; when_: 't option; (* side condition *) expr: 't }
[@@deriving yojson]

val pattern_ty : pattern -> Type.t
val mk : loc:Iloc.t -> t_view -> t
val mk_pat : loc:Iloc.t -> pattern_view -> pattern
val Type.t : t -> Type.t
val Var.t : loc:Iloc.t -> Var.t -> t
val apply : loc:Iloc.t -> Type.t:Type.t -> t -> apply_arg list -> t
val const : loc:Iloc.t -> Type.t:Type.t -> const -> t
val let_ : loc:Iloc.t -> flg:rec_flag -> binding list -> t -> t
val tuple : loc:Iloc.t -> Type.t:Type.t -> t list -> t
val if_ : loc:Iloc.t -> t -> t -> t -> t
val as_ : loc:Iloc.t -> t -> Type.t -> t
val equal_const : const -> const -> bool
val equal_id : Uid.t -> Uid.t -> bool
val equal_var : Var.t -> Var.t -> bool
val equal_ty : Type.t -> Type.t -> bool
val equal : t -> t -> bool

