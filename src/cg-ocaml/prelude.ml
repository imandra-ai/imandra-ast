type cbor = CBOR.Simple.t

exception Cbor_error of cbor * string

let cbor_error c s = raise (Cbor_error (c, s))

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

  let of_cbor (c : cbor) : t =
    match c with
    | `Tag (30, `Array [ num; den ]) ->
      let num = DJ_Z.of_cbor num and den = DJ_Z.of_cbor den in
      Q.make num den
    | _ -> cbor_error c "expected Q.t (tag 30, [num,den])"
end

open Imandra_prelude

[@@@ocaml.warning "-39"]
