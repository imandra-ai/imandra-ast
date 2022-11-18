open Imandra_prelude

module DJ_Z = struct
  type t = Z.t

  let to_yojson t = `Int (Z.to_int t)

  let of_yojson = function
    | `Int i -> Ok (Z.of_int i)
    | _ -> Error "DJ_Z.of_yojson"
end

module DJ_Q = struct
  type t = Q.t

  let to_yojson t = `Float (Q.to_float t)

  let of_yojson = function
    | `Float f -> Ok (Q.of_float f)
    | _ -> Error "DJ_Q.of_yojson"
end
