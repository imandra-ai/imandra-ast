type t = {
  name: string;  (** name. unique for persistent, not unique for generative *)
  id: int;
}
[@@deriving yojson, show, eq, ord]

let hash x = CCHash.(combine2 (string x.name) (int x.id))
let n_ = CCAtomic.make 0 (* generator for IDs *)
let name self = self.name
let id self = self.id

(* persistent Uid: use -1 as id *)
let make_persistent name : t = { name; id = -1 }

let make name : t =
  let id = CCAtomic.fetch_and_add n_ 1 in
  { name; id }

let fresh_copy self = make self.name

module As_key = struct
  type nonrec t = t

  let equal = equal
  let hash = hash
  let compare = compare
end

module Tbl = CCHashtbl.Make (As_key)
module Map = CCMap.Make (As_key)
module Set = CCSet.Make (As_key)
