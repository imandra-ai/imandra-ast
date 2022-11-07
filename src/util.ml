(* Some simple utilities useful for Imandra *)

open Unix
module Fmt = CCFormat

type 'a iter = ('a -> unit) -> unit

module Str_map = CCMap.Make (String)
module Str_set = CCSet.Make (String)
module Str_tbl = CCHashtbl.Make (CCString)
module Int_tbl = CCHashtbl.Make (CCInt)
module Int_set = CCSet.Make (CCInt)
module Int_map = CCMap.Make (CCInt)

let pp_int fmt (i : int) = Format.fprintf fmt "%di" i
let pp_float fmt (f : float) = Format.fprintf fmt "%sp" (string_of_float f)

let pp_list ?(sep = "") ppx out l =
  Fmt.(list ~sep:(fun out () -> Fmt.fprintf out "%s@ " sep) ppx) out l

let pp_iter ?(sep = "") ppx out it =
  Fmt.(iter ~sep:(fun out () -> Fmt.fprintf out "%s@ " sep) ppx) out it

let pp_text_newlines out (s : string) : unit =
  Format.fprintf out "@[<v>";
  List.iteri
    (fun i s ->
      if i > 0 then Format.fprintf out "@,";
      Format.pp_print_string out s)
    (CCString.lines s);
  Format.fprintf out "@]"

let split_path s =
  let rec aux s i =
    if i >= 0 && s.[i] = '.' then
      (* drop trailing dots *)
      aux s (i - 1)
    else (
      match String.rindex_from s i '.' with
      | j ->
        let rhs = String.sub s (j + 1) (String.length s - j - 1) in
        let lhs = String.split_on_char '.' @@ String.sub s 0 j in
        (lhs, rhs)
      | exception Not_found -> ([], s)
    )
  in
  aux s (String.length s - 1)

let join_path x y =
  if x = [] then
    y
  else
    String.concat "." x ^ "." ^ y

let chop_path s = snd @@ split_path s
let is_qualified s = fst @@ split_path s <> []
