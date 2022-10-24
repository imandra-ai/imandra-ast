(** Location *)

type t = {
  filename: string;
  line_start: int;
  line_end: int;
}
[@@deriving yojson, show, eq, ord]

val filename : t -> string
val line_start : t -> int
val line_end : t -> int
val make : filename:string -> line_start:int -> line_end:int -> unit -> t
