open Common_

exception Error of string

let error msg : 'a = raise (Error msg)
let errorf fmt = Fmt.kasprintf error fmt
