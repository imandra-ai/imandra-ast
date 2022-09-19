type t = { id: Uid.t; ty: Type.t } [@@deriving show, eq, ord, yojson]

let id self = self.id
let name self = Uid.name self.id
let ty self = self.ty
let make ~ty id : t = { ty; id }
let make_str ~ty id : t = { ty; id = Uid.make id }
let fresh_copy self = { self with id = Uid.fresh_copy self.id }
let hash self = CCHash.(combine2 (Uid.hash self.id) (Type.hash self.ty))
