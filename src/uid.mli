(** Unique IDs

    Uids are used to uniquely identify types, variables, functions, etc. *)

type t = private {
  name: string;  (** name. unique for persistent, not unique for generative *)
  id: int;
}
[@@deriving yojson, show, eq, ord]

val hash : t -> int
val make : string -> t
val make_persistent : string -> t
val fresh_copy : t -> t
val name : t -> string
val id : t -> int

module Tbl : CCHashtbl.S with type key = t
module Map : CCMap.S with type key = t
module Set : CCSet.S with type elt = t
