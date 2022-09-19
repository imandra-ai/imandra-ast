type t = { filename: string; line_start: int; line_end: int }
[@@deriving yojson, show, eq]

let filename self = self.filename
let line_start self = self.line_start
let line_end self = self.line_end

let make ~filename ~line_start ~line_end () : t =
  { filename; line_start; line_end }
