module Str_tbl = CCHashtbl.Make (CCString)
module Fmt = CCFormat

let spf = Printf.sprintf
let fpf = Fmt.fprintf
let[@inline] ( let@ ) f x = f x

type state = {
  code: Buffer.t;
  ty_defs: Type.Defs.t;
  cstor_labels: Uid.t list Uid.Tbl.t; (* cstor -> its labels *)
  uids: string Uid.Tbl.t;
  seen: unit Str_tbl.t;
  mutable gen: int;
}

type kind =
  | K_ty_var
  | K_cstor
  | K_field
  | K_fun
  | K_ty
  | K_var
  | K_mod

let prelude = Prelude_code.code

(* generate unique name from [base] and counter *)
let gensym (self : state) (base : string) : string =
  let new_ () =
    let n = self.gen in
    self.gen <- n + 1;
    n
  in
  let rec loop i =
    let s =
      if i = 0 then
        base
      else
        spf "%s%d" base (new_ ())
    in
    if Str_tbl.mem self.seen s then
      loop (i + 1)
    else
      s
  in
  loop 0

(** find or create OCaml symbol for this ID *)
let str_of_id (self : state) (id : Uid.t) (kind : kind) : string =
  Uid.Tbl.get_or_add self.uids ~k:id ~f:(fun id ->
      (* FIXME: escape OCaml keywords *)
      let name = Uid.name id in
      let mod_name, base_name = Util.split_path name in
      let base, must_be_unique =
        match kind with
        | K_ty ->
          let l =
            match (mod_name, base_name) with
            | [], _ -> [ base_name ]
            | _, "t" -> mod_name
            | _ -> mod_name @ [ base_name ]
          in
          (String.uncapitalize_ascii @@ String.concat "__" l, true)
        | K_mod -> (String.capitalize_ascii base_name, true)
        | K_cstor -> (String.capitalize_ascii base_name, false)
        | K_ty_var ->
          (* remove the "'" *)
          ( String.capitalize_ascii (String.sub name 1 (String.length name - 1)),
            true )
        | K_field -> (String.uncapitalize_ascii base_name, false)
        | K_var -> (String.uncapitalize_ascii base_name, true)
        | K_fun -> (String.uncapitalize_ascii base_name, true)
      in
      let s =
        if must_be_unique then
          gensym self base
        else
          base
      in
      Str_tbl.add self.seen s ();
      Uid.Tbl.add self.uids id s;
      s)

(** Allocate local formatter, add the content to the code buffer *)
let with_local_fmt (self : state) (f : Fmt.t -> unit) : unit =
  let buf = Buffer.create 32 in
  let out = Format.formatter_of_buffer buf in
  Fmt.fprintf out "@[<2>";
  let@ () =
    Fun.protect ~finally:(fun () ->
        Fmt.fprintf out "@]@.";
        Buffer.add_buffer self.code buf)
  in
  f out

let str_of_cg (self : state) (cg : state -> Buffer.t -> 'a -> unit) (x : 'a) :
    string =
  let buf = Buffer.create 32 in
  cg self buf x;
  Buffer.contents buf

(** Emit type *)
let cg_ty ?(clique = []) (self : state) (out : Fmt.t) (ty : Type.t) : unit =
  let rec recurse out ty =
    match Type.view ty with
    | Type.Var v ->
      let str = str_of_id self v K_var in
      fpf out "%s" str
    | Type.Arrow (lbl, a, b) ->
      if lbl <> "" then fpf out "%s:" lbl;
      fpf out "(@[%a ->@ %a@])" recurse a recurse b
    | Type.Tuple l ->
      fpf out "(@[";
      List.iteri
        (fun i a ->
          if i > 0 then fpf out " *@ ";
          recurse out a)
        l;
      fpf out "@])"
    | Type.Constr (c, []) ->
      let repr =
        (* use deriving-yojson module wrappers to support
           serialization of these [int] and [real] *)
        match Uid.name c with
        | "int" -> "DJ_Z.t"
        | "real" -> "DJ_Q.t"
        | _name -> str_of_id self c K_ty
      in

      fpf out "%s" repr
    | Type.Constr (c, args) ->
      fpf out "(@[";
      List.iteri
        (fun i a ->
          if i > 0 then fpf out ",@ ";
          recurse out a)
        args;
      fpf out "@]) %s" (str_of_id self c K_ty)
  in

  let ty = Type.chase_deep self.ty_defs ty in
  recurse out ty

let cg_const (out : Fmt.t) (c : Term.const) : unit =
  match c with
  | Term.Const_nativeint n -> fpf out "%ndn" n
  | Term.Const_int32 n -> fpf out "%ldl" n
  | Term.Const_int64 n -> fpf out "%LdL" n
  | Term.Const_float f -> fpf out "%h" f
  | Term.Const_char c -> fpf out "%C" c
  | Term.Const_string s -> fpf out "%S" s
  | Term.Const_z s -> fpf out "(Z.of_string %S)" (Z.to_string s)
  | Term.Const_q q -> fpf out "(Q.of_string %S)" (Q.to_string q)
  | Term.Const_real_approx _s ->
    fpf out "assert false (* TODO: real approx %s *)" _s

let cg_pat (self : state) (out : Fmt.t) (p : Term.pattern) : unit =
  let rec recurse out (p : Term.pattern) : unit =
    match p.view with
    | Term.P_var v -> fpf out "%s" (str_of_id self v.id K_var)
    | Term.P_tuple (_, l) ->
      fpf out "(@[";
      List.iteri
        (fun i x ->
          if i > 0 then fpf out ",@ ";
          recurse out x)
        l;
      fpf out "@])"
    | Term.P_any _ -> fpf out "_"
    | Term.P_true -> fpf out "true"
    | Term.P_false -> fpf out "false"
    | Term.P_const (c, _) -> cg_const out c
    | Term.P_or (a, b) -> fpf out "(@[<hv>%a@ | %a@])" recurse a recurse b
    | Term.P_construct { c; args = []; _ } ->
      fpf out "%s" (str_of_id self c K_cstor)
    | Term.P_construct { c; args; lbls = None; _ } ->
      fpf out "%s(@[" (str_of_id self c K_cstor);
      List.iteri
        (fun i ty ->
          if i > 0 then fpf out ",@ ";
          recurse out ty)
        args;
      fpf out "@])"
    | Term.P_construct { c; args; lbls = Some lbls; _ } ->
      assert false (* TODO *)
    | Term.P_record (_, rows) ->
      fpf out "{@[";
      List.iteri
        (fun i (f, p) ->
          if i > 0 then fpf out ";@ ";
          fpf out "@[<1>%s:@ %a@]" (str_of_id self f K_field) recurse p)
        rows;
      fpf out "@]}"
    | Term.P_alias (v, p) ->
      fpf out "(@[<1>%a@ as %s@])" recurse p (str_of_id self v.id K_var)
  in
  recurse out p

let str_of_apply_arg = function
  | Term.Nolabel -> ""
  | Term.Label s -> spf "~%s" s
  | Term.Optional s -> spf "?%s" s

let is_base_infix s =
  match s.[0] with
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> false
  | _ -> true

let is_infix s =
  let base = Util.chop_path s in
  base = "" || is_base_infix base

let rec cg_term (self : state) (out : Fmt.t) (t : Term.t) : unit =
  let rec recurse out (t : Term.t) : unit =
    match Term.view t with
    | Term.Const (c, _) -> cg_const out c
    | Term.If (a, b, c) ->
      fpf out "(@[if %a@ then@ %a@ else %a@])" recurse a recurse b recurse c
    | Term.Let (rec_, bs, bod) ->
      fpf out "@[@[<hv>let%s "
        (if rec_ = Term.Recursive then
          " rec"
        else
          "");
      List.iteri
        (fun i ((x : Var.t), t) ->
          if i > 0 then fpf out "@ and ";
          fpf out "@[<1>%s =@ %a@]" (str_of_id self x.id K_var) recurse t)
        bs;
      fpf out "@] in@ %a@]" recurse bod
    | Term.Apply (_, f, []) -> recurse out f
    | Term.Apply (_, { Term.view = Term.Ident f; _ }, [ a; b ])
      when is_infix (Uid.name f.id) && not (Util.is_qualified (Uid.name f.id))
      ->
      fpf out "(@[%a %s@ %a@])" recurse_arg a (Uid.name f.id) recurse_arg b
    | Term.Apply (_, f, args) ->
      fpf out "(@[%a" recurse f;
      List.iter (fun a -> fpf out "@ %a" recurse_arg a) args;
      fpf out "@])"
    | Term.Ident x ->
      let s = str_of_id self x.id K_var in
      (* make sure to protect partially applied infix symbols and the likes *)
      let components, base = Util.split_path s in
      let s =
        if is_base_infix base then (
          (* enclose an infix function base with parens *)
          let base = spf "( %s )" base in
          Util.join_path components base
        ) else
          s
      in
      fpf out "%s" s
    | Term.Tuple (_, l) ->
      fpf out "(@[";
      List.iteri
        (fun i x ->
          if i > 0 then fpf out ",@ ";
          recurse out x)
        l;
      fpf out "@])"
    | Term.True -> fpf out "true"
    | Term.False -> fpf out "false"
    | Term.Fun (_, lbl, x, bod) ->
      fpf out "(@[fun %s%s ->@ %a@])" (str_of_apply_arg lbl)
        (str_of_id self x.id K_var)
        recurse bod
    | Term.Field { f; t; _ } ->
      fpf out "%a.%s" recurse t (str_of_id self f K_field)
    | Term.Construct { c; args = []; _ } ->
      fpf out "%s" (str_of_id self c K_cstor)
    | Term.Construct { c; args; lbls = None; _ } ->
      fpf out "(@[<2>%s(" (str_of_id self c K_cstor);
      List.iteri
        (fun i x ->
          if i > 0 then fpf out ",@ ";
          recurse out x)
        args;
      fpf out ")@])"
    | Term.Construct { c; args; lbls = Some lbls; _ } -> assert false (* TODO *)
    | Term.Record (_, rows, rest) ->
      (match rest with
      | None -> fpf out "@[{"
      | Some r -> fpf out "{@[(%a) with@ " recurse r);
      List.iteri
        (fun i (f, x) ->
          if i > 0 then fpf out ";@ ";
          fpf out "@[<1>%s =@ %a@]" (str_of_id self f K_field) recurse x)
        rows;
      fpf out "@]}"
    | Term.As (t, ty) -> fpf out "(@[%a : %a@])" recurse t (cg_ty self) ty
    | Term.Match { lhs; bs; _ } ->
      fpf out "(@[<hv>match %a with" recurse lhs;
      List.iter (fun vb -> fpf out "@ @[| %a@]" (cg_vb self) vb) bs;
      fpf out "@])"
    | Term.Let_match { flg; bs; body; _ } ->
      fpf out "@[@[<hv>let%s "
        (match flg with
        | Term.Recursive -> " rec"
        | _ -> "");
      List.iteri
        (fun i vb ->
          if i > 0 then fpf out "@ and ";
          fpf out "%a" (cg_vb self) vb)
        bs;
      fpf out "@] in@ %a@]" recurse body
  and recurse_arg out (arg, a) =
    let s = str_of_apply_arg arg in
    fpf out " %s%a" s recurse a
  in

  recurse out t

and cg_vb (self : state) out (vb : Term.t Term.vb) : unit =
  let { Term.pat; when_; expr } = vb in
  fpf out "@[<1>@[<2>%a" (cg_pat self) pat;
  Option.iter (fun e -> fpf out "@ when %a@]" (cg_term self) e) when_;
  fpf out " ->@ %a@]" (cg_term self) expr

let cg_ty_decl (self : state) ~clique (out : Fmt.t) (ty_def : Type.def) : unit =
  let name = str_of_id self ty_def.name K_ty in
  let args =
    match ty_def.params with
    | [] -> ""
    | l ->
      spf "(%s)" @@ String.concat ","
      @@ List.map (fun tyv -> str_of_id self tyv K_ty_var) l
  in

  (match ty_def.decl with
  | Type.Record rows ->
    fpf out "@[<hv2>type %s%s = {@ " name args;
    List.iter
      (fun { Type.f; ty } ->
        fpf out "@[<1>%s:@ %a@];@ " (str_of_id self f K_field)
          (cg_ty ~clique self) ty)
      rows;

    fpf out "@]}"
  | Type.Algebraic cstors ->
    fpf out "@[<hv2>type %s%s =@ " name args;
    List.iter
      (fun { Type.c; args; labels } ->
        fpf out "@[| %s" (str_of_id self c K_cstor);
        (match (args, labels) with
        | [], _ -> ()
        | _, None ->
          fpf out " of ";
          List.iteri
            (fun i a ->
              if i > 0 then fpf out "@ * ";
              cg_ty ~clique self out a)
            args
        | _, Some lbls ->
          assert (List.length lbls = List.length args);
          fpf out " of ";
          Uid.Tbl.add self.cstor_labels c lbls;
          fpf out "{@[";
          List.iter2
            (fun lbl a ->
              fpf out "@[<1>%s:@ %a@];@ "
                (str_of_id self lbl K_field)
                (cg_ty ~clique self) a)
            lbls args;
          fpf out "@]}");
        fpf out "@]@ ")
      cstors
  | Type.Builtin _ -> assert false (* TODO *)
  | Type.Alias { target } ->
    fpf out "@[<2>type %s%s =@ " name args;
    cg_ty ~clique self out target;
    fpf out "@]"
  | Type.Other | Type.Skolem ->
    fpf out "@[<v>(* (other) *)@ @[type %s%s@]@]" name args);
  fpf out "%s" "[@@deriving yojson]"

let cg_fun (self : state) ~sep (out : Fmt.t) (f : Term.fun_decl) : unit =
  fpf out "@[<hv2>%s %a =@ %a@]" sep (cg_pat self) f.pat (cg_term self) f.body

let rec cg_decl (self : state) (out : Fmt.t) (d : Decl.t) : unit =
  match d.view with
  | Decl.Ty { tys = defs } ->
    let clique = List.map (fun d -> d.Type.name) defs in
    List.iter (cg_ty_decl self ~clique out) defs
  | Decl.Fun { recursive; fs } ->
    fpf out "@[<hv>";
    List.iteri
      (fun i f ->
        let sep =
          if i = 0 then
            if recursive then
              "let rec"
            else
              "let"
          else
            "and"
        in
        cg_fun ~sep self out f)
      fs;
    fpf out "@]"
  | Decl.Module_alias (name, m) ->
    fpf out "module %s = %s"
      (str_of_id self name K_mod)
      (str_of_id self m K_mod)
  | Decl.Module { name; items } ->
    fpf out "module %s = struct" (str_of_id self name K_mod);
    List.iter (cg_decl self out) items;
    fpf out "end"

let codegen (decls : Decl.t list) : string =
  let ty_defs = Decl.ty_defs_of_decls decls in
  let st =
    {
      code = Buffer.create 64;
      seen = Str_tbl.create 8;
      ty_defs;
      cstor_labels = Uid.Tbl.create 16;
      uids = Uid.Tbl.create 32;
      gen = 1;
    }
  in
  Printf.bprintf st.code "(* generated from imandra-ast *)\n%s\n" prelude;
  List.iter
    (fun d ->
      let@ out = with_local_fmt st in
      fpf out "%a@." (cg_decl st) d)
    decls;
  Buffer.contents st.code
