(** Location *)

type t = private { filename: string; line_start: int; line_end: int }
[@@deriving yojson, show, eq]

val filename : t -> string
val line_start : t -> int
val line_end : t -> int
val make : filename:string -> line_start:int -> line_end:int -> unit -> t
