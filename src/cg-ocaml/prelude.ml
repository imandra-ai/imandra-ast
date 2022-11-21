type cbor = CBOR.Simple.t

module DJ_Z = struct
  type t = Z.t

  let to_yojson t = `Int (Z.to_int t)

  let of_yojson = function
    | `Int i -> Ok (Z.of_int i)
    | _ -> Error "DJ_Z.of_yojson"

  (* tag 2 (bytes) https://www.rfc-editor.org/rfc/rfc8949.html#name-bignums *)
  let to_cbor (self : t) : cbor =
    (* small number: just use CBOR representation *)
    if Z.(abs self < of_int Stdlib.(1 lsl 62)) then
      `Int (Z.to_int self)
    else if Z.sign self >= 0 then
      `Tag (2, `Bytes (Z.to_bits self))
    else
      `Tag (3, `Bytes (Z.to_bits Z.(sub minus_one self)))

  let of_cbor (c : cbor) : _ result =
    match c with
    | `Int i -> Ok (Z.of_int i)
    | `Tag (2, `Bytes s) ->
      (try Ok (Z.of_bits s) with e -> Error "invalid bytes for Z.t (tag 2)")
    | `Tag (3, `Bytes s) ->
      (try
         let n = Z.of_bits s in
         Ok Z.(minus_one - n)
       with e -> Error "invalid bytes for Z.t (tag 3)")
    | _ -> Error "expected Z.3 (tag 2 or 3)"
end

module DJ_Q = struct
  type t = Q.t

  let to_yojson t = `Float (Q.to_float t)

  let of_yojson = function
    | `Float f -> Ok (Q.of_float f)
    | _ -> Error "DJ_Q.of_yojson"

  (* tag 30, [num,den] http://peteroupc.github.io/CBOR/rational.html *)
  let to_cbor (self : t) : cbor =
    let num = Q.num self and den = Q.den self in
    `Tag (30, `Array [ DJ_Z.to_cbor num; DJ_Z.to_cbor den ])

  let of_cbor (c : cbor) : _ result =
    match c with
    | `Tag (30, `Array [ num; den ]) ->
      (match (DJ_Z.of_cbor num, DJ_Z.of_cbor den) with
      | Ok a, Ok b -> Ok (Q.make a b)
      | Error e, _ | _, Error e -> Error e)
    | _ -> Error "expected Q.t (tag 30, [num,den])"
end

open Imandra_prelude
