(** AST to emit OCaml easily  *)

module Fmt = CCFormat

let spf = Printf.sprintf
let fpf = Format.fprintf

(** Expression used for code generation *)
module Expr = struct
  type expr_lbl =
    [ `Nolabel
    | `Lbl of string
    | `Optional of string
    ]

  type 'a fun_lbl =
    [ `Nolabel
    | `Lbl
    | `Optional of 'a
    ]

  type t =
    | E_const of int
    | E_add of t * t
    | E_mult of t * t
    | E_app of t * t list
    | E_of of string * t list
    | E_tyapp of string * t list
    | E_cstor_app of string * t list
    | E_seq of t list (* ; *)
    | E_list of t list (* list literal *)
    | E_comment of string * t
    | E_fun of unit fun_lbl * string * t option * t
    | E_let of [ `Rec of bool ] * (t * t) list * t
    | E_var of string (* local id *)
    | E_glob of string (* global constant *)
    | E_raw of string (* raw code, be careful *)
    | E_attr of t * string
    | E_deref of t
    | E_if of t * t * t
    | E_upcast of t * t (* expr:>ty *)
    | E_as of t * string (* e as foo *)
    | E_field of t * string
    | E_ty_lbl of string * t (* labelled type arg *)
    | E_lbl of expr_lbl * t (* labelled arg *)
    | E_infix of string * t * t
    | E_infix_noparen of string * t * t
    | E_for of t * t * t * t
    | E_vbar of t list
    | E_vbar_arrow of (t * t) list
    | E_when of t * t
    | E_match of t * t
    | E_tuple of string * t list
    | E_block of string * t * string
    | E_rectype of (string * t) list
    | E_rec of (string * t) list * t option
    | E_beginend of t

  let str_of_fun_lbl : _ fun_lbl -> string = function
    | `Nolabel -> ""
    | `Lbl -> "~"
    | `Optional _ -> "?"

  let rec pp_top out = function
    | E_const i -> fpf out "%d" i
    | E_var s | E_glob s -> Fmt.string out s
    | E_ty_lbl (s, e) -> Fmt.fprintf out "%s:@[%a@]" s pp e
    | E_lbl (`Nolabel, e) -> pp out e
    | E_lbl (`Lbl s, e) -> Fmt.fprintf out "~%s:@[%a@]" s pp e
    | E_lbl (`Optional s, e) -> Fmt.fprintf out "?%s:@[%a@]" s pp e
    | E_attr (e, a) -> Fmt.fprintf out "@[%a@ %s@]" pp e a
    | E_when (a, b) -> fpf out "@[%a@ when %a@]" pp a pp b
    | E_of (c, []) -> Fmt.string out c
    | E_of (c, l) ->
      Fmt.fprintf out "%s of %a" c Fmt.(list ~sep:(return " *@ ") pp) l
    | E_tuple (sep, l) ->
      let pp_sep out () = Fmt.fprintf out "%s@ " sep in
      Fmt.fprintf out "%a" Fmt.(list ~sep:pp_sep pp) l
    | E_list l ->
      Fmt.fprintf out "[@[%a@]]"
        Fmt.(list ~sep:(return ";@ ") @@ hovbox pp_top)
        l
    | E_app (f, []) -> pp out f
    | E_app (f, args) ->
      fpf out "@[<1>%a@ %a@]" pp f Fmt.(list ~sep:(return "@ ") pp) args
    | E_cstor_app (c, []) -> fpf out "%s" c
    | E_cstor_app (c, [ x ]) -> fpf out "@[<1>%s@ %a@]" c pp x
    | E_cstor_app (c, l) ->
      fpf out "@[<1>%s (@[%a@])@]" c Fmt.(list ~sep:(return ",@ ") pp) l
    | E_tyapp (s, []) -> Fmt.string out s
    | E_tyapp (s, l) ->
      Fmt.fprintf out "(%a) %s" Fmt.(list ~sep:(return ",@ ") pp) l s
    | E_seq l -> fpf out "(@[<v>%a@])" Fmt.(list ~sep:(return ";@ ") pp) l
    | E_let (_, [], bod) -> pp out bod
    | E_let (`Rec rec_, b1 :: bs, bod) ->
      let rec_ =
        if rec_ then
          " rec"
        else
          ""
      in
      fpf out "@[<v>";
      let pp1 ~pre out (v, t) = fpf out "@[<2>%s %a =@ %a@]" pre pp v pp t in
      pp1 ~pre:("let" ^ rec_) out b1;
      List.iter (pp1 ~pre:"and" out) bs;
      fpf out " in@ %a@]" pp bod
    | E_fun (lbl, v, None, body) ->
      fpf out "(@[fun %s%s ->@ %a@])" (str_of_fun_lbl lbl) v pp body
    | E_fun (lbl, v, Some ty, body) ->
      fpf out "(@[fun %s(@[%s:%a@])@ -> %a@])" (str_of_fun_lbl lbl) v pp ty pp
        body
    | E_for (i, a, b, bod) ->
      fpf out "@[@[<hv2>for %a = %a to %a do@ %a@]@ done@]" pp i pp a pp b pp
        bod
    | E_upcast (e, ty) -> fpf out "(@[%a :> %a@])" pp e pp ty
    | E_as (e, s) -> fpf out "(@[%a as %s@])" pp e s
    | E_field (e, f) -> fpf out "@[%a@].%s" pp e f
    | E_raw r ->
      let ls = String.split_on_char '\n' r in
      Fmt.fprintf out "@[<v>%a@]" Fmt.(list ~sep:(return "@ ") string) ls
    | E_add (a, b) -> fpf out "(@[%a@ + %a@])" pp a pp b
    | E_mult (a, b) -> fpf out "(@[%a@ * %a@])" pp a pp b
    | E_deref v -> fpf out "!@[%a@]" pp v
    | E_if (a, b, c) -> fpf out "(@[if %a@ then %a@ else %a@])" pp a pp b pp c
    | E_comment (s, e) -> fpf out "(* %s *)@ %a" s pp e
    | E_infix (s, a, b) -> fpf out "(@[%a %s@ %a@])" pp a s pp b
    | E_infix_noparen (s, a, b) -> fpf out "@[%a %s@ %a@]" pp a s pp b
    | E_block (a, b, c) -> fpf out "@[<v>@[<v2>%s@ %a@]@ %s@]" a pp b c
    | E_vbar_arrow l ->
      List.iter (fun (x, y) -> fpf out "@ | @[%a ->@ %a@]" pp x pp y) l
    | E_match (lhs, rhs) ->
      fpf out "(@[<v>@[<2>match@ %a with@]%a@])" pp lhs pp rhs
    | E_vbar l -> List.iter (fun x -> fpf out "@;| @[%a@]" pp x) l
    | E_rectype l ->
      let pppair out (s, ty) = fpf out "@[<2>%s:@ %a@]" s pp ty in
      fpf out "{@[<hv>%a@]}" Fmt.(list ~sep:(return ";@ ") pppair) l
    | E_rec (l, rest) ->
      let pppair out (s, e) =
        match e with
        | E_var s' when s = s' -> fpf out "%s" s (* puning *)
        | _ -> fpf out "@[<2>%s =@ %a@]" s pp e
      in
      (match rest with
      | None -> fpf out "{@[<hv>%a@]}" Fmt.(list ~sep:(return ";@ ") pppair) l
      | Some rest ->
        fpf out "{@[<hv>(%a) with@ %a@]}" pp rest
          Fmt.(list ~sep:(return ";@ ") pppair)
          l)
    | E_beginend t -> fpf out "@[@[<2>begin@ %a@]@ end@]" pp_top t

  and pp out = function
    | ( E_tuple _
      | E_app (_, _ :: _)
      | E_cstor_app (_, _ :: _)
      | E_tyapp (_, _ :: _) ) as e ->
      fpf out "(%a)" pp_top e
    | e -> pp_top out e

  let to_string : t -> string = Fmt.to_string pp
  let const i = E_const i
  let zero = const 0
  let deref v = E_deref v
  let ty_app s l : t = E_tyapp (s, l)
  let var s = E_var s
  let var_f fmt = Fmt.kasprintf var fmt
  let attr ~a e : t = E_attr (e, a)
  let glob s = E_glob s
  let glob_f fmt = Fmt.kasprintf glob fmt
  let comment s e = E_comment (s, e)
  let if_ a b c : t = E_if (a, b, c)
  let vbar l = E_vbar l
  let vbar_arrow l = E_vbar_arrow l
  let list_ l = E_list l
  let match_ e l : t = E_match (e, l)
  let for_ i a b bod : t = E_for (i, a, b, bod)
  let beginend a : t = E_beginend a
  let record_type l : t = E_rectype l
  let record ?rest l : t = E_rec (l, rest)
  let field e f : t = E_field (e, f)
  let of_ s l : t = E_of (s, l)

  let seq = function
    | [] -> glob "()"
    | [ x ] -> x
    | l -> E_seq l

  let fun_ ?(lbl = `Nolabel) v ?ty body = E_fun (lbl, v, ty, body)

  let rec fun_l ?lbl l bod =
    match l with
    | [] -> bod
    | (x, ty) :: tl -> fun_ ?lbl x ?ty @@ fun_l ?lbl tl bod

  let app s l =
    if l = [] then
      s
    else
      E_app (s, l)

  let app_var s l = app (var s) l
  let app_cstor s l = E_cstor_app (s, l)
  let app_glob s l = app (glob s) l
  let ( ||> ) a b = app b [ a ]
  let ( @@@ ) f x = app f [ x ]

  let upcast e ty =
    match e with
    | E_const _ -> e
    | _ -> E_upcast (e, ty)

  let as_ e x : t = E_as (e, x)
  let raw s = E_raw s
  let block ~enter:a b ~exit:c = E_block (a, b, c)
  let lbl s e = E_lbl (`Lbl s, e)
  let lbl_opt s e = E_lbl (`Optional s, e)
  let ty_lbl s e = E_ty_lbl (s, e)
  let raw_f fmt = Fmt.kasprintf raw fmt
  let infix s a b = E_infix (s, a, b)
  let infix_no_paren s a b = E_infix_noparen (s, a, b)
  let when_ a b : t = E_when (a, b)

  (* no simps *)
  let ( - ) a b = infix "-" a b
  let ( @-> ) a b = infix "->" a b
  let ( --> ) a b = infix_no_paren "->" a b
  let ( := ) a b = infix ":=" a b
  let cast a b = infix ":" a b
  let tuple_ty l : t = E_tuple ("*", l)
  let tuple l : t = E_tuple (",", l)

  let ( + ) a b =
    match (a, b) with
    | E_const 0, b -> b
    | a, E_const 0 -> a
    | _ -> infix "+" a b

  let ( * ) a b =
    match (a, b) with
    | E_const 1, b -> b
    | a, E_const 1 -> a
    | _ -> infix "*" a b

  let let_l ?(rec_ = false) l bod : t = E_let (`Rec rec_, l, bod)

  let let_ ?rec_ v t u =
    match u with
    | E_var v' ->
      if v = v' then
        t
      else
        u
    | _ -> let_l ?rec_ [ (var v, t) ] u

  let let_pat ?rec_ pat t u : t = let_l ?rec_ [ (pat, t) ] u

  let cstruct_sub ba off =
    app_glob "Cstruct.sub"
      [
        upcast ba (glob "Cstruct.t");
        lbl "off" off;
        lbl "len"
          (app_glob "Cstruct.length" [ upcast ba (glob "Cstruct.t") ] - off);
      ]
end

type expr = Expr.t
type size = expr
type offset = expr
type code = expr

module Decl = struct
  module Fmt = CCFormat

  type var = Expr.t option Expr.fun_lbl * string * Expr.t option

  type t =
    | D_mod of t list option * string * t list
    | D_mod_enter of string
    | D_mod_exit
    | D_newline
    | D_doc of string * t
    | D_attr of t * string
    | D_let of [ `Rec of bool ] * (Expr.t * Expr.t option * Expr.t) list
    | D_ty of (string * string list * Expr.t) list
    | D_val of string * Expr.t
    | D_raw of string

  let let_l ?(rec_ = false) l : t =
    let r = `Rec rec_ in
    D_let (r, l)

  let let_ ?rec_ f args ?ret body : t =
    let_l ?rec_ [ (Expr.app_var f args, ret, body) ]

  let ty s vars bod : t = D_ty [ (s, vars, bod) ]
  let ty_l l : t = D_ty l
  let val_ s ty : t = D_val (s, ty)
  let mod_ ?sig_ m l : t = D_mod (sig_, m, l)
  let mod_enter m : t = D_mod_enter m
  let mod_exit : t = D_mod_exit
  let newline = D_newline
  let raw s : t = D_raw s
  let doc s d : t = D_doc (s, d)
  let attr ~a d : t = D_attr (d, a)

  let pp_var out ((lbl, s, ty) : var) =
    match (lbl, ty) with
    | `Optional None, None -> Fmt.fprintf out "?%s" s
    | `Optional (Some e), None -> Fmt.fprintf out "?(@[%s = %a@])" s Expr.pp e
    | `Optional (Some e), Some ty ->
      Fmt.fprintf out "?(@[%s:%a = %a@])" s Expr.pp ty Expr.pp e
    | _, None -> Fmt.fprintf out "%s%s" (Expr.str_of_fun_lbl lbl) s
    | _, Some ty ->
      Fmt.fprintf out "%s(@[%s@ : %a@])" (Expr.str_of_fun_lbl lbl) s Expr.pp ty

  let rec pp out (self : t) : unit =
    match self with
    | D_mod (sig_, name, l) ->
      let pp_sig out =
        match sig_ with
        | None -> ()
        | Some s ->
          Fmt.fprintf out " : sig@ %a@ end " Fmt.(list ~sep:(return "@ ") pp) s
      in
      Fmt.fprintf out "@[<v>@[<v2>@[<v2>module %s%t@] = struct@ %a@]@ end@]"
        name pp_sig
        Fmt.(list ~sep:(return "@,@,") pp)
        l
    | D_mod_enter name -> Fmt.fprintf out "@[<v>@[<v2>module %s = struct@," name
    | D_mod_exit -> Fmt.fprintf out "@]@,end@]"
    | D_doc (s, sub) ->
      Fmt.fprintf out "@[<v>(** %a *)@ %a@]" Fmt.string_lines s pp sub
    | D_attr (d, attr) -> Fmt.fprintf out "@[<v>%a@ %s@]" pp d attr
    | D_let (_, []) -> assert false
    | D_let (`Rec rec_, f1 :: fs) ->
      let pp_rec =
        if rec_ then
          " rec"
        else
          ""
      in
      let pp_ret out = function
        | None -> ()
        | Some ty -> Fmt.fprintf out "@ : %a" Expr.pp ty
      in
      let pp1 ~pre (pat, ret, bod) =
        Fmt.fprintf out "@[<2>%s @[<2>%a@,%a@] =@ %a@]" pre Expr.pp_top pat
          pp_ret ret Expr.pp bod
      in
      pp1 ~pre:("let" ^ pp_rec) f1;
      List.iter (pp1 ~pre:"and") fs
    | D_val (s, ty) -> Fmt.fprintf out "@[<hv2>val %s : %a@]" s Expr.pp ty
    | D_ty [] -> assert false
    | D_ty (ty1 :: tys) ->
      let pp1 ~pre (s, vars, rhs) =
        let lhs = Expr.(ty_app s (List.map var vars)) in
        Fmt.fprintf out "@[<hv2>%s %a =@ %a@]" pre Expr.pp lhs Expr.pp rhs
      in
      Fmt.fprintf out "@[<hv>";
      pp1 ~pre:"type" ty1;
      List.iter (pp1 ~pre:"and") tys;
      Fmt.fprintf out "@]"
    | D_raw s -> Fmt.fprintf out "@[%a@]" Fmt.text s
    | D_newline -> Fmt.fprintf out ""

  let to_string : t -> string = Fmt.to_string pp
end
