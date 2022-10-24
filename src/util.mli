(** Utilities *)

open Common_

type 'a iter = ('a -> unit) -> unit

module Str_map : CCMap.S with type key = string
module Str_set : CCSet.S with type elt = string
module Str_tbl : CCHashtbl.S with type key = string
module Int_tbl : CCHashtbl.S with type key = int
module Int_set : CCSet.S with type elt = int
module Int_map : CCMap.S with type key = int

val pp_int : int Fmt.printer
val pp_float : float Fmt.printer
val pp_list : ?sep:string -> 'a Fmt.printer -> 'a list Fmt.printer
val pp_iter : ?sep:string -> 'a Fmt.printer -> 'a iter Fmt.printer

val pp_text_newlines : string Fmt.printer
(** Print string in a vbox with newlines replaced with ["@,"],
    but does not touch spaces unlike {!Format.pp_print_text} *)

val chop_path : string -> string
(** Remove module prefix, turning [A.B.c] into [c] *)

val split_path : string -> string list * string
(** Split into module prefix and identifier *)

val join_path : string list * string -> string
(** Revere of {!split_path} *)
