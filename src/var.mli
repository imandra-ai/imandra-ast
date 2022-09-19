(** Variables *)

type t = private { id: Uid.t; ty: Type.t } [@@deriving show, eq, ord]

val make : ty:Type.t -> Uid.t -> t
val make_str : ty:Type.t -> string -> t
val fresh_copy : t -> t
val hash : t -> int
