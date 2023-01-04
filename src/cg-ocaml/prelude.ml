type cbor = CBOR.Simple.t

exception Cbor_error of cbor * string

let cbor_error c s = raise (Cbor_error (c, s))

module BuiltinsSerde_Z = struct
  type t = Z.t

  let to_yojson t = `Int (Z.to_int t)

  let of_yojson = function
    | `Int i -> Ok (Z.of_int i)
    | _ -> Error "BuiltinsSerde_Z.of_yojson"

  (* tag 2 (bytes) https://www.rfc-editor.org/rfc/rfc8949.html#name-bignums *)
  let to_cbor (self : t) : cbor =
    (* small number: just use CBOR representation *)
    if Z.(abs self < of_int Stdlib.(1 lsl 62)) then
      `Int (Z.to_int self)
    else if Z.sign self >= 0 then
      `Tag (2, `Bytes (Z.to_bits self))
    else
      `Tag (3, `Bytes (Z.to_bits Z.(sub minus_one self)))

  let of_cbor (c : cbor) : t =
    match c with
    | `Int i -> Z.of_int i
    | `Tag (2, `Bytes s) ->
      (try Z.of_bits s with _ -> cbor_error c "invalid bytes for Z.t (tag 2)")
    | `Tag (3, `Bytes s) ->
      (try
         let n = Z.of_bits s in
         Z.(minus_one - n)
       with _ -> cbor_error c "invalid bytes for Z.t (tag 3)")
    | _ -> cbor_error c "expected Z.3 (tag 2 or 3)"
end

module BuiltinsSerde_Q = struct
  type t = Q.t

  let to_yojson t = `Float (Q.to_float t)

  let of_yojson = function
    | `Float f -> Ok (Q.of_float f)
    | _ -> Error "BuiltinsSerde_Q.of_yojson"

  (* tag 30, [num,den] http://peteroupc.github.io/CBOR/rational.html *)
  let to_cbor (self : t) : cbor =
    let num = Q.num self and den = Q.den self in
    `Tag
      (30, `Array [ BuiltinsSerde_Z.to_cbor num; BuiltinsSerde_Z.to_cbor den ])

  let of_cbor (c : cbor) : t =
    match c with
    | `Tag (30, `Array [ num; den ]) ->
      let num = BuiltinsSerde_Z.of_cbor num
      and den = BuiltinsSerde_Z.of_cbor den in
      Q.make num den
    | _ -> cbor_error c "expected Q.t (tag 30, [num,den])"
end

module BuiltinsSerde_String = struct
  type t = string

  let to_cbor (self : t) : cbor = `Text self

  let of_cbor (c : cbor) : t =
    match c with
    | `Text s -> s
    | _ -> cbor_error c "expected string"
end

module BuiltinsSerde_Bool = struct
  type t = bool

  let to_cbor (self : t) : cbor = `Bool self

  let of_cbor (c : cbor) : t =
    match c with
    | `Bool b -> b
    | _ -> cbor_error c "expected bool"
end

open Imandra_prelude

module BuiltinsSerde_List = struct
  type 'a t = 'a list

  let to_cbor f (self : _ t) : cbor = `Array (Caml.List.map f self)

  let of_cbor f (c : cbor) : _ t =
    match c with
    | `Array l -> List.map f l
    | _ -> cbor_error c "expected option"
end

module BuiltinsSerde_Option = struct
  type 'a t = 'a option

  let to_cbor f (self : _ t) : cbor =
    match self with
    | None -> `Null
    | Some x -> `Array [ f x ]

  let of_cbor f (c : cbor) : _ t =
    match c with
    | `Null -> None
    | `Array [ x ] -> Some (f x)
    | _ -> cbor_error c "expected option"
end

module BuiltinsSerde_Result = struct
  type ('a, 'b) t = ('a, 'b) result

  let to_cbor fok ferr (self : _ t) : cbor =
    match self with
    | Ok x -> `Map [ (`Text "Ok", fok x) ]
    | Error e -> `Map [ (`Text "Err", ferr e) ]

  let of_cbor fok ferr (c : cbor) : _ t =
    match c with
    | `Map [ (`Text "Ok", x) ] -> Ok (fok x)
    | `Map [ (`Text "Err", e) ] -> Error (ferr e)
    | _ -> cbor_error c "expected result"
end

[@@@ocaml.warning "-39"]
