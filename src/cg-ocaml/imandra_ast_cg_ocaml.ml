module Str_tbl = CCHashtbl.Make (CCString)
module Fmt = CCFormat

let spf = Printf.sprintf
let bpf = Printf.bprintf
let[@inline] ( let@ ) f x = f x

type state = {
  out: Buffer.t;
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

let prelude = {ocaml|
open Imandra_prelude;;
|ocaml}

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
      let base =
        match kind with
        | K_ty ->
          let l =
            match (mod_name, base_name) with
            | [], _ -> [ base_name ]
            | _, "t" -> mod_name
            | _ -> mod_name @ [ base_name ]
          in
          String.concat "" l
        | K_mod | K_cstor ->
          Util.join_path mod_name (String.capitalize_ascii base_name)
        | K_ty_var ->
          (* remove the "'" *)
          String.capitalize_ascii (String.sub name 1 (String.length name - 1))
        | K_field ->
          Util.join_path mod_name @@ String.uncapitalize_ascii base_name
        | K_var ->
          Util.join_path mod_name @@ String.uncapitalize_ascii base_name
        | K_fun ->
          Util.join_path mod_name @@ String.uncapitalize_ascii base_name
      in
      let s = gensym self base in
      Str_tbl.add self.seen s ();
      Uid.Tbl.add self.uids id s;
      s)

let str_of_cg (self : state) (cg : state -> Buffer.t -> 'a -> unit) (x : 'a) :
    string =
  let buf = Buffer.create 32 in
  cg self buf x;
  Buffer.contents buf

let cg_ty ?(clique = []) (self : state) (out : Buffer.t) (ty : Type.t) : unit =
  let rec recurse out ty =
    match Type.view ty with
    | Type.Var v ->
      let str = str_of_id self v K_var in
      bpf out "%s" str
    | Type.Arrow (lbl, a, b) ->
      if lbl <> "" then bpf out "%s:" lbl;
      bpf out "(%a -> %a)" recurse a recurse b
    | Type.Tuple l ->
      bpf out "(";
      List.iteri
        (fun i a ->
          if i > 0 then bpf out "*";
          recurse out a)
        l;
      bpf out ")"
    | Type.Constr (c, []) ->
      let repr =
        match Uid.name c with
        | "int" -> "Z.t"
        | "real" -> "Q.t"
        | _name -> str_of_id self c K_ty
      in

      bpf out "%s" repr
    | Type.Constr (c, args) ->
      bpf out "(";
      List.iteri
        (fun i a ->
          if i > 0 then bpf out ",";
          recurse out a)
        args;
      bpf out ") %s" (str_of_id self c K_ty)
  in

  let ty = Type.chase_deep self.ty_defs ty in
  recurse out ty

let cg_const (out : Buffer.t) (c : Term.const) : unit =
  match c with
  | Term.Const_nativeint n -> bpf out "%ndn" n
  | Term.Const_int32 n -> bpf out "%ldl" n
  | Term.Const_int64 n -> bpf out "%LdL" n
  | Term.Const_float f -> bpf out "%h" f
  | Term.Const_char c -> bpf out "%C" c
  | Term.Const_string s -> bpf out "%S" s
  | Term.Const_z s -> bpf out "(Z.of_string %S)" (Z.to_string s)
  | Term.Const_q q -> bpf out "(Q.of_string %S)" (Q.to_string q)
  | Term.Const_real_approx _s ->
    bpf out "assert false (* TODO: real approx %s *)" _s

let cg_pat (self : state) (out : Buffer.t) (p : Term.pattern) : unit =
  let rec recurse out (p : Term.pattern) : unit =
    match p.view with
    | Term.P_var v -> bpf out "%s" (str_of_id self v.id K_var)
    | Term.P_tuple (_, l) ->
      bpf out "(";
      List.iteri
        (fun i x ->
          if i > 0 then bpf out ",";
          recurse out x)
        l;
      bpf out ")"
    | Term.P_any _ -> bpf out "_"
    | Term.P_true -> bpf out "true"
    | Term.P_false -> bpf out "false"
    | Term.P_const (c, _) -> cg_const out c
    | Term.P_or (a, b) -> bpf out "(%a | %a)" recurse a recurse b
    | Term.P_construct { c; args = []; _ } ->
      bpf out "%s" (str_of_id self c K_cstor)
    | Term.P_construct { c; args; lbls = None; _ } ->
      bpf out "%s(" (str_of_id self c K_cstor);
      List.iteri
        (fun i ty ->
          if i > 0 then bpf out ",";
          recurse out ty)
        args;
      bpf out ")"
    | Term.P_construct { c; args; lbls = Some lbls; _ } ->
      assert false (* TODO *)
    | Term.P_record (_, rows) ->
      bpf out "{";
      List.iteri
        (fun i (f, p) ->
          if i > 0 then bpf out ";";
          bpf out "%s: %a" (str_of_id self f K_field) recurse p)
        rows;
      bpf out "}"
    | Term.P_alias (v, p) ->
      bpf out "(%a as %s)" recurse p (str_of_id self v.id K_var)
  in
  recurse out p

let str_of_apply_arg = function
  | Term.Nolabel -> ""
  | Term.Label s -> spf "~%s" s
  | Term.Optional s -> spf "?%s" s

let is_infix s =
  let s = Util.chop_path s in
  s = ""
  ||
  match s.[0] with
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> false
  | _ -> true

let rec cg_term (self : state) (out : Buffer.t) (t : Term.t) : unit =
  let rec recurse out (t : Term.t) : unit =
    match Term.view t with
    | Term.Const (c, _) -> cg_const out c
    | Term.If (a, b, c) ->
      bpf out "(if %a\n then %a\n else %a)" recurse a recurse b recurse c
    | Term.Let (rec_, bs, bod) ->
      bpf out "let%s "
        (if rec_ = Term.Recursive then
          " rec"
        else
          "");
      List.iteri
        (fun i ((x : Var.t), t) ->
          if i > 0 then bpf out " and ";
          bpf out "%s = %a\n" (str_of_id self x.id K_var) recurse t)
        bs;
      bpf out "in\n%a" recurse bod
    | Term.Apply (_, f, []) -> recurse out f
    | Term.Apply (_, { Term.view = Term.Ident f; _ }, [ a; b ])
      when is_infix (Uid.name f.id) && not (Util.is_qualified (Uid.name f.id))
      ->
      bpf out "(%a %s %a)" recurse_arg a (Uid.name f.id) recurse_arg b
    | Term.Apply (_, f, args) ->
      bpf out "(%a" recurse f;
      List.iter (fun a -> bpf out " %a" recurse_arg a) args;
      bpf out ")"
    | Term.Ident x ->
      let s = str_of_id self x.id K_var in
      (* make sure to protect partially applied infix symbols and the likes *)
      let s =
        if is_infix s then
          spf "(%s)" s
        else
          s
      in
      bpf out "%s" s
    | Term.Tuple (_, l) ->
      bpf out "(";
      List.iteri
        (fun i x ->
          if i > 0 then bpf out ", ";
          recurse out x)
        l;
      bpf out ")"
    | Term.True -> bpf out "true"
    | Term.False -> bpf out "false"
    | Term.Fun (_, lbl, x, bod) ->
      bpf out "(fun %s%s ->\n %a)" (str_of_apply_arg lbl)
        (str_of_id self x.id K_var)
        recurse bod
    | Term.Field { f; t; _ } ->
      bpf out "%a.%s" recurse t (str_of_id self f K_field)
    | Term.Construct { c; args = []; _ } ->
      bpf out "%s" (str_of_id self c K_cstor)
    | Term.Construct { c; args; lbls = None; _ } ->
      bpf out "%s(" (str_of_id self c K_cstor);
      List.iteri
        (fun i x ->
          if i > 0 then bpf out ", ";
          recurse out x)
        args;
      bpf out ")"
    | Term.Construct { c; args; lbls = Some lbls; _ } -> assert false (* TODO *)
    | Term.Record (_, rows, rest) ->
      (match rest with
      | None -> bpf out "{"
      | Some r -> bpf out "{(%a) with " recurse r);
      List.iteri
        (fun i (f, x) ->
          if i > 0 then bpf out "; ";
          bpf out "%s=%a" (str_of_id self f K_field) recurse x)
        rows;
      bpf out "}"
    | Term.As (t, ty) -> bpf out "(%a : %a)" recurse t (cg_ty self) ty
    | Term.Match { lhs; bs; _ } ->
      bpf out "(match %a with\n" recurse lhs;
      List.iter (fun vb -> bpf out "| %a\n" (cg_vb self) vb) bs;
      bpf out ")"
    | Term.Let_match { flg; bs; body; _ } ->
      bpf out "let%s "
        (match flg with
        | Term.Recursive -> " rec"
        | _ -> "");
      List.iteri
        (fun i vb ->
          if i > 0 then bpf out "\n and ";
          bpf out "%a" (cg_vb self) vb)
        bs;
      bpf out "in %a" recurse body
  and recurse_arg out (arg, a) =
    let s = str_of_apply_arg arg in
    bpf out " %s%a" s recurse a
  in

  recurse out t

and cg_vb (self : state) out (vb : Term.t Term.vb) : unit =
  let { Term.pat; when_; expr } = vb in
  bpf out "%a" (cg_pat self) pat;
  Option.iter (fun e -> bpf out " when %a" (cg_term self) e) when_;
  bpf out " -> %a" (cg_term self) expr

let cg_ty_decl (self : state) ~clique (out : Buffer.t) (ty_def : Type.def) :
    unit =
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
    bpf out "type %s%s = {\n" name args;
    List.iter
      (fun { Type.f; ty } ->
        bpf out "  %s: %a;\n" (str_of_id self f K_field) (cg_ty ~clique self) ty)
      rows;

    bpf out "}"
  | Type.Algebraic cstors ->
    bpf out "type %s%s = \n" name args;
    List.iter
      (fun { Type.c; args; labels } ->
        bpf out "  | %s of " (str_of_id self c K_cstor);
        (match (args, labels) with
        | [], _ -> ()
        | _, None ->
          List.iteri
            (fun i a ->
              if i > 0 then bpf out " * ";
              cg_ty ~clique self out a)
            args
        | _, Some lbls ->
          assert (List.length lbls = List.length args);
          Uid.Tbl.add self.cstor_labels c lbls;
          bpf out "{\n";
          List.iter2
            (fun lbl a ->
              bpf out "%s: %a,"
                (str_of_id self lbl K_field)
                (cg_ty ~clique self) a)
            lbls args);
        bpf out "\n")
      cstors
  | Type.Builtin _ -> assert false (* TODO *)
  | Type.Alias { target } ->
    bpf out "type %s%s = " name args;
    cg_ty ~clique self out target
  | Type.Other | Type.Skolem -> bpf out "(* (other) *)\ntype %s%s;" name args);
  bpf out "\n\n"

let cg_fun (self : state) ~sep (out : Buffer.t) (f : Term.fun_decl) : unit =
  bpf out "%s %a = %a\n" sep (cg_pat self) f.pat (cg_term self) f.body

let rec cg_decl (self : state) (d : Decl.t) : unit =
  match d.view with
  | Decl.Ty { tys = defs } ->
    let clique = List.map (fun d -> d.Type.name) defs in
    List.iter (cg_ty_decl self ~clique self.out) defs
  | Decl.Fun { recursive; fs } ->
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
        cg_fun ~sep self self.out f)
      fs;
    bpf self.out "\n"
  | Decl.Module_alias (name, m) ->
    bpf self.out "module %s = %s"
      (str_of_id self name K_mod)
      (str_of_id self m K_mod)
  | Decl.Module { name; items } ->
    bpf self.out "module %s = struct\n" (str_of_id self name K_mod);
    List.iter (cg_decl self) items;
    bpf self.out "end"

let codegen (decls : Decl.t list) : string =
  let ty_defs = Decl.ty_defs_of_decls decls in
  let st =
    {
      out = Buffer.create 64;
      seen = Str_tbl.create 8;
      ty_defs;
      cstor_labels = Uid.Tbl.create 16;
      uids = Uid.Tbl.create 32;
      gen = 1;
    }
  in
  bpf st.out "(* generated from imandra-ast *)\n%s\n" prelude;
  List.iter (cg_decl st) decls;
  Buffer.contents st.out
