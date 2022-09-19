(** Types and type definitions *)

open Common_

module Var : module type of Uid
(** Type variables *)

type var = Var.t [@@deriving yojson, show, eq, ord]
(** Variable, including ['a] *)

type 't adt_row = {
  c: Uid.t;
  labels: Uid.t list option;  (** for inline records; same length as [args] *)
  args: 't list;
}
[@@deriving yojson, show]
(** List of constructors for an algebraic type *)

type 't rec_row = {
  f: Uid.t;
  ty: 't;
}
[@@deriving yojson, show]
(** List of record fields *)

type +'t view = private
  | Var of var
  | Arrow of string * 't * 't
  | Tuple of 't list
  | Constr of Uid.t * 't list
[@@deriving yojson, eq, ord, map, iter, show]

type t [@@deriving yojson, show, eq, ord]
(** A general type expression for Imandra *)

val view : t -> t view
(** View for a type *)

(** Definition of a named type *)
type 't decl =
  | Algebraic of 't adt_row list
  | Record of 't rec_row list
  | Alias of { target: 't }
  | Skolem
  | Builtin of string
  | Other
[@@deriving yojson, eq, show]

type def = private {
  params: var list;
  decl: t decl;
  name: Uid.t;
  name_loc: Loc.t;  (** loc of [name] *)
  loc: Loc.t;
  codegen_tags: (string * string) list;
}
[@@deriving show, yojson]
(** A type definition, parametrized by some type variables *)

(** A set of definitions *)
module Defs : sig
  type t [@@deriving show]

  val empty : t
  val add : def -> t -> t
  val find : t -> Uid.t -> def option
  val find_exn : t -> Uid.t -> def
  val keys : t -> Uid.t list
end

val mk_def :
  ?codegen_tags:(string * string) list ->
  loc:Loc.t ->
  name_loc:Loc.t ->
  name:Uid.t ->
  params:var list ->
  t decl ->
  def

val equal_chase_deep : Defs.t -> t -> t -> bool
(** Chase deep + type equality *)

val hash : t -> int
(** Hash type *)

val pp_as_ocaml : t Fmt.printer

(** {2 Creation} *)

val mk_var : Var.t -> t
val mk_arrow_l : t list -> t -> t
val mk_arrow : ?lbl:string -> t -> t -> t
val as_arrow : t -> (string * t * t) option
val as_arrow2 : t -> (t * t * t) option
val mk_constr : Uid.t -> t list -> t
val mk_constr0 : Uid.t -> t
val mk_tuple : t list -> t

val mk_skolem : Uid.t -> t
(** Skolem type *)

val in_out_types : Defs.t -> t -> t list * t

val in_out_types_no_chase : t -> t list * t
(** Open the toplevel function types *)

val n_args : Defs.t -> t -> int
(** Number of arguments for a function type *)

val mk_other_def : loc:Loc.t -> Uid.t -> def
(** Undefined type *)

val mk_skolem_def : loc:Loc.t -> Uid.t -> def
(** Definition of a skolem *)

val returns : Defs.t -> t -> t
(** Return type of a function (same as [snd (in_out_types ty)]) *)

val drop_n_args : Defs.t -> int -> t -> t
(** Assuming the type has at least [n] arguments, remove the n first.
    Normally one should have [drop_n_args (n_args ty) = returns ty] *)

val is_param : Defs.t -> t -> bool
(** Parametric type (higher order, or polymorphic)? *)

module Subst : sig
  type ty = t

  type nonrec t = t Var.Map.t [@@deriving show]
  (** Substitution: a binding from variables to types *)

  val empty : t

  val apply : ?rec_:bool -> ty -> t -> ty
  (** Apply substitution
    @param rec_ if true (default), substitute in RHS of substitution too. *)

  val apply' : ?rec_:bool -> t -> ty -> ty
end

val free_vars : t -> Var.Set.t
(** Set of variables of the type *)

val free_vars_add : Var.Set.t -> t -> Var.Set.t
(** add free vars of the type to this set *)

val is_ground : t -> bool
(** Is the type ground (i.e. without variables)? *)

val is_poly : t -> bool
(** Is the type non-ground (i.e. with some variables)? *)

val is_ground_def : def -> bool
(** Is the definition parameter-free? *)

val name : def -> Uid.t

val is_inline_record_cstor : def -> Uid.t -> bool
(** [is_inline_record_cstor def c] returns [true] if [c] is a constructor
    of [def] with an inline record *)

val find_cstor_exn : def -> Uid.t -> t adt_row
(** [find_cstor def c] returns the description of [c] where [c] is a
    constructor of [def].
    @raise Not_found otherwise *)

val find_cstor_by_name_exn : def -> string -> t adt_row

val cstors_of_def : def -> Uid.t list
(** List constructors of this datatype *)

val head_def_of_ty : Defs.t -> t -> def
(** Obtain the definition for a type [Constr (def, args)].
    @raise Error.Error if the type is not a defined type. *)

val iter_def_uids : def -> (Uid.t -> unit) -> unit
(** Iterate on the IDs in the definition *)

val fold_def_uids : ('a -> Uid.t -> 'a) -> 'a -> def -> 'a
(** Fold on the IDs in the definition *)

val iter_shallow : t -> (t -> unit) -> unit
(** Iterate on immediate sub-types *)

val map_shallow : t -> f:(t -> t) -> t
(** Map on immediate sub-types *)

val chase : Defs.t -> ?subst:Subst.t -> t -> t
(** follow bound variables and aliases at the root of the type.
    The returned type is either an arrow, a non-bound variable or
    a non-alias Constr. *)

val chase_deep : Defs.t -> ?subst:Subst.t -> t -> t
(** Deep version of {!chase}, normalizing arguments recursively *)

val same_base_type : Defs.t -> t -> t -> bool
(** Same head constructor? *)

module Tbl : CCHashtbl.S with type key = t
module Map : CCMap.S with type key = t

(** {2 Some builtins} *)

val bool : t
