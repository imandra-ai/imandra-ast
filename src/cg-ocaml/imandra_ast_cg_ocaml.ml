module Str_tbl = CCHashtbl.Make (CCString)
module Fmt = CCFormat
module A = Ocaml_ast
module E = A.Expr

let spf = Printf.sprintf
let fpf = Fmt.fprintf
let[@inline] ( let@ ) f x = f x

type code_stack = {
  mutable decls: A.Decl.t list;
  parent: (string * code_stack) option; (* when inside a module with this name *)
}

type kind =
  | K_ty_var
  | K_cstor
  | K_field
  | K_fun
  | K_ty
  | K_ty_to_cbor
  | K_ty_of_cbor
  | K_var
  | K_mod
[@@deriving eq]

module Uid_kind_tbl = CCHashtbl.Make (struct
  type t = Uid.t * kind [@@deriving eq]

  let hash = Hashtbl.hash
end)

type state = {
  mutable code: code_stack;
  ty_defs: Type.Defs.t;
  cstor_labels: Uid.t list Uid.Tbl.t; (* cstor -> its labels *)
  uids: string Uid_kind_tbl.t;
  seen: unit Str_tbl.t;
  mutable gen: int;
}

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
  Uid_kind_tbl.get_or_add self.uids ~k:(id, kind) ~f:(fun (id, kind) ->
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
        | K_ty_to_cbor ->
          let base_name =
            if base_name = "t" then
              "to_cbor"
            else
              "cbor_of_" ^ base_name
          in
          (String.uncapitalize_ascii base_name, true)
        | K_ty_of_cbor ->
          let base_name =
            if base_name = "t" then
              "of_cbor"
            else
              "cbor_to_" ^ base_name
          in
          (String.uncapitalize_ascii base_name, true)
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
      Uid_kind_tbl.add self.uids (id, kind) s;
      s)

let add_decl (self : state) (d : A.Decl.t) =
  self.code.decls <- d :: self.code.decls

let with_module (self : state) ~mod_name (f : unit -> 'a) : 'a =
  let old_code = self.code in
  let st = { decls = []; parent = Some (mod_name, old_code) } in
  Fun.protect f ~finally:(fun () ->
      self.code <- old_code;
      (* push module *)
      add_decl self (A.Decl.mod_ mod_name (List.rev st.decls)))

(** Emit type *)
let cg_ty ?(clique = []) (self : state) (ty : Type.t) : E.t =
  let rec recurse ty : E.t =
    match Type.view ty with
    | Type.Var v ->
      let str = str_of_id self v K_var in
      E.var str
    | Type.Arrow (lbl, a, b) ->
      let a = recurse a and b = recurse b in
      let a =
        if lbl <> "" then
          E.ty_lbl lbl a
        else
          a
      in
      E.(a --> b)
    | Type.Tuple l -> E.tuple_ty (List.map recurse l)
    | Type.Constr (c, []) ->
      let repr =
        (* use deriving-yojson module wrappers to support
           serialization of these [int] and [real] *)
        match Uid.name c with
        | "int" -> "DJ_Z.t"
        | "real" -> "DJ_Q.t"
        | _name -> str_of_id self c K_ty
      in
      E.var repr
    | Type.Constr (c, args) ->
      E.ty_app (str_of_id self c K_ty) (List.map recurse args)
  in

  let ty = Type.chase_deep self.ty_defs ty in
  recurse ty

let cg_const (c : Term.const) : E.t =
  E.raw
  @@
  match c with
  | Term.Const_nativeint n -> spf "%ndn" n
  | Term.Const_int32 n -> spf "%ldl" n
  | Term.Const_int64 n -> spf "%LdL" n
  | Term.Const_float f -> spf "%h" f
  | Term.Const_char c -> spf "%C" c
  | Term.Const_string s -> spf "%S" s
  | Term.Const_z s -> spf "(Z.of_string %S)" (Z.to_string s)
  | Term.Const_q q -> spf "(Q.of_string %S)" (Q.to_string q)
  | Term.Const_real_approx _s ->
    spf "assert false (* TODO: real approx %s *)" _s

let rec cg_pat (self : state) (p : Term.pattern) : E.t =
  let recurse = cg_pat self in
  match p.view with
  | Term.P_var v -> E.var (str_of_id self v.id K_var)
  | Term.P_tuple (_, l) -> E.tuple (List.map recurse l)
  | Term.P_any _ -> E.raw "_"
  | Term.P_true -> E.var "true"
  | Term.P_false -> E.var "false"
  | Term.P_const (c, _) -> cg_const c
  | Term.P_or (a, b) -> E.vbar [ recurse a; recurse b ]
  | Term.P_construct { c; args; lbls = None; _ } ->
    E.app_cstor (str_of_id self c K_cstor) (List.map recurse args)
  | Term.P_construct { c; args; lbls = Some lbls; _ } -> assert false (* TODO *)
  | Term.P_record (_, rows) ->
    E.record
      (List.map (fun (f, p) -> (str_of_id self f K_field, recurse p)) rows)
  | Term.P_alias (v, p) -> E.as_ (recurse p) (str_of_id self v.id K_var)

let is_base_infix s =
  match s.[0] with
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> false
  | _ -> true

let is_infix s =
  let base = Util.chop_path s in
  base = "" || is_base_infix base

let rec cg_term (self : state) (t : Term.t) : E.t =
  let rec recurse t : E.t =
    match Term.view t with
    | Term.Const (c, _) -> cg_const c
    | Term.If (a, b, c) -> E.if_ (recurse a) (recurse b) (recurse c)
    | Term.Let (rec_, bs, bod) ->
      let bs =
        List.map
          (fun (v, t) -> (E.var @@ str_of_id self v.Var.id K_var, recurse t))
          bs
      in
      let bod = recurse bod in
      E.let_l ~rec_:(rec_ = Term.Recursive) bs bod
    | Term.Apply (_, f, []) -> recurse f
    | Term.Apply (_, { Term.view = Term.Ident f; _ }, [ a; b ])
      when is_infix (Uid.name f.id) && not (Util.is_qualified (Uid.name f.id))
      ->
      E.infix (Uid.name f.id) (recurse_arg a) (recurse_arg b)
    | Term.Apply (_, f, args) -> E.app (recurse f) (List.map recurse_arg args)
    | Term.Ident x ->
      let s = str_of_id self x.id K_var in
      (* make sure to protect partially applied infix symbols and the likes *)
      let components, base = Util.split_path s in
      E.var
      @@
      if is_base_infix base then (
        (* enclose an infix function base with parens *)
        let base = spf "( %s )" base in
        Util.join_path components base
      ) else
        s
    | Term.Tuple (_, l) -> E.tuple (List.map recurse l)
    | Term.True -> E.var "true"
    | Term.False -> E.var "false"
    | Term.Fun (_, lbl, x, bod) ->
      let lbl =
        match lbl with
        | Term.Optional s -> `Optional ()
        | Term.Nolabel -> `Nolabel
        | Term.Label _ -> `Lbl
      in

      E.fun_ ~lbl (str_of_id self x.id K_var) (recurse bod)
    | Term.Field { f; t; _ } -> E.field (recurse t) (str_of_id self f K_field)
    | Term.Construct { c; args; lbls = None; _ } ->
      E.app_cstor (str_of_id self c K_cstor) (List.map recurse args)
    | Term.Construct { c; args; lbls = Some lbls; _ } ->
      (* TODO *)
      E.raw_f "assert false (* TODO: construct with labels *)"
    | Term.Record (_, rows, rest) ->
      let rows =
        List.map (fun (f, x) -> (str_of_id self f K_field, recurse x)) rows
      in
      E.record ?rest:(Option.map recurse rest) rows
    | Term.As (t, ty) -> E.cast (recurse t) (cg_ty self ty)
    | Term.Match { lhs; bs; _ } ->
      let bs = List.map (cg_vb self) bs |> E.vbar_arrow in
      E.match_ (recurse lhs) bs
    | Term.Let_match { flg; bs; body; _ } ->
      let bs = List.map (cg_vb self) bs in
      let body = recurse body in
      E.let_l ~rec_:(flg = Term.Recursive) bs body
  and recurse_arg (arg, a) : E.t =
    let e = recurse a in
    match arg with
    | Term.Nolabel -> e
    | Term.Label s -> E.lbl s e
    | Term.Optional s -> E.lbl_opt s e
  in

  recurse t

and cg_vb (self : state) (vb : Term.t Term.vb) : E.t * E.t =
  let { Term.pat; when_; expr } = vb in
  let pat = cg_pat self pat in
  let when_ = Option.map (cg_term self) when_ in
  let rhs = cg_term self expr in
  let lhs =
    match when_ with
    | Some e -> E.when_ pat e
    | _ -> pat
  in
  (lhs, rhs)

let cg_ty_decl (self : state) ~clique (ty_def : Type.def) :
    string * string list * E.t =
  let name = str_of_id self ty_def.name K_ty in
  let args = List.map (fun tyv -> str_of_id self tyv K_ty_var) ty_def.params in

  let rhs, can_derive =
    match ty_def.decl with
    | Type.Record rows ->
      let rows =
        List.map
          (fun { Type.f; ty } ->
            (str_of_id self f K_field, (cg_ty ~clique self) ty))
          rows
      in
      (E.record_type rows, true)
    | Type.Algebraic cstors ->
      let conv_cstor { Type.c; args; labels } =
        let c = str_of_id self c K_cstor in
        match (args, labels) with
        | [], _ -> E.var c
        | _, None -> E.of_ c @@ List.map (cg_ty self ~clique) args
        | _, Some lbls ->
          assert (List.length lbls = List.length args);
          E.of_ c
            [
              E.record_type
              @@ List.map2
                   (fun lbl a ->
                     (str_of_id self lbl K_field, (cg_ty ~clique self) a))
                   lbls args;
            ]
      in
      let cstors = List.map conv_cstor cstors in
      (E.vbar cstors, true)
    | Type.Builtin _ -> assert false (* TODO *)
    | Type.Alias { target } ->
      let can_derive = not (Type.is_arrow self.ty_defs target) in
      (cg_ty self ~clique target, can_derive)
    | Type.Other | Type.Skolem ->
      let code = E.comment "(other)" @@ E.raw (String.capitalize_ascii name) in
      (code, false)
  in

  let rhs =
    if can_derive then
      E.attr rhs ~a:"[@@deriving yojson]"
    else
      rhs
  in
  (name, args, rhs)

let cg_ty_to_cbor (self : state) ~clique (ty : Type.t) (expr : E.t) : E.t =
  let rec recurse ty expr : E.t =
    match Type.view ty with
    | Type.Var v ->
      let str = str_of_id self v K_ty_to_cbor in
      E.app_var str [ expr ]
    | Type.Arrow _ -> E.raw_f "assert false (* cannot encode arrow *)"
    | Type.Tuple l ->
      E.let_l [ (E.tuple @@ List.mapi (fun i _ -> E.var_f "_x_%d" i) l, expr) ]
      @@ E.app_cstor "`List"
      @@ List.mapi (fun i ty -> recurse ty (E.var_f "_x_%d" i)) l
    | Type.Constr (c, []) ->
      let repr =
        (* use deriving-yojson module wrappers to support
           serialization of these [int] and [real] *)
        match Uid.name c with
        | "int" -> "DJ_Z.to_cbor"
        | "real" -> "DJ_Q.to_cbor"
        | _name -> str_of_id self c K_ty_to_cbor
      in
      E.app_var repr [ expr ]
    | Type.Constr (c, args) ->
      let f = str_of_id self c K_ty_to_cbor in
      (* FIXME: pass arguments *)
      E.app_var f [ expr ]
  in

  let ty = Type.chase_deep self.ty_defs ty in
  recurse ty expr

let cg_ty_decl_to_cbor (self : state) ~clique (ty_def : Type.def) :
    E.t * _ * E.t =
  let name = str_of_id self ty_def.name K_ty_to_cbor in

  let expr_self = E.var "self" in
  let args =
    List.map (fun tyv -> E.var @@ str_of_id self tyv K_var) ty_def.params
    @ [ E.cast expr_self (E.var @@ str_of_id self ty_def.name K_ty) ]
  in

  let rhs =
    match ty_def.decl with
    | Type.Alias { target } -> cg_ty_to_cbor self ~clique target expr_self
    | Type.Record rows ->
      let rows =
        List.map
          (fun { Type.f; ty } ->
            let f =
              E.app_cstor "`Text" [ E.string_lit (str_of_id self f K_field) ]
            and expr_self = E.field expr_self (str_of_id self f K_field) in
            E.tuple [ f; cg_ty_to_cbor ~clique self ty expr_self ])
          rows
      in
      E.app_cstor "`Map" [ E.list_ rows ]
    | Type.Algebraic cstors ->
      let conv_cstor { Type.c; args; labels } =
        let c = str_of_id self c K_cstor in
        let cbor_c_as_text = E.app_cstor "`Text" [ E.string_lit c ] in
        match (args, labels) with
        | [], _ -> E.(var c --> cbor_c_as_text)
        | _, None ->
          let vars = List.mapi (fun i _ -> E.var_f "_x_%d" i) args in
          let cbor_args =
            [
              cbor_c_as_text;
              E.app_var "`Array"
                [
                  E.list_
                  @@ List.mapi
                       (fun i ty ->
                         cg_ty_to_cbor self ~clique ty (E.var_f "_x_%d" i))
                       args;
                ];
            ]
          in
          E.(app_cstor c vars --> app_cstor "`Array" [ E.list_ cbor_args ])
        | _, Some lbls ->
          assert (List.length lbls = List.length args);
          let pat =
            E.app_cstor c
              [
                E.record
                @@ List.mapi
                     (fun i lbl ->
                       (str_of_id self lbl K_field, E.var_f "_x_%d" i))
                     lbls;
              ]
          in
          let cbor_args =
            [
              cbor_c_as_text;
              E.app_cstor "`Map"
              @@ List.mapi
                   (fun i ty ->
                     let lbl = List.nth lbls i in
                     E.tuple
                       [
                         E.app_cstor "`Text"
                           [ E.string_lit @@ str_of_id self lbl K_field ];
                         cg_ty_to_cbor self ~clique ty (E.var_f "_x_%d" i);
                       ])
                   args;
            ]
          in
          E.(pat --> app_cstor "`Array" [ E.list_ cbor_args ])
      in
      let cases = E.vbar @@ List.map conv_cstor cstors in
      E.match_ expr_self cases
    | _ -> E.raw " assert false (* TODO *)"
    (* TODO *)
    (*
    | Type.Other | Type.Skolem ->
      let code = E.comment "(other)" @@ E.raw (String.capitalize_ascii name) in
      (code, false)
      *)
  in

  (E.app_var name args, Some (E.var "cbor"), rhs)

let cg_ty_of_cbor (self : state) ~clique (ty : Type.t) (expr : E.t) : E.t =
  let rec recurse ty expr : E.t =
    match Type.view ty with
    | Type.Var v ->
      let str = str_of_id self v K_ty_of_cbor in
      E.app_var str [ expr ]
    | Type.Arrow _ -> E.raw_f "assert false (* cannot decode arrow type *)"
    | Type.Tuple l ->
      E.(
        match_ expr @@ vbar
        @@ [
             app_cstor "`List"
               [ list_ (List.mapi (fun i ty -> var_f "_x_%d" i) l) ]
             --> tuple (List.mapi (fun i ty -> recurse ty (var_f "_x_%d" i)) l);
             raw "_"
             --> app_var "cbor_error" [ expr; string_lit "expected tuple" ];
           ])
    | Type.Constr (c, []) ->
      let repr =
        (* use deriving-yojson module wrappers to support
           serialization of these [int] and [real] *)
        match Uid.name c with
        | "int" -> "DJ_Z.of_cbor"
        | "real" -> "DJ_Q.of_cbor"
        | _name -> str_of_id self c K_ty_of_cbor
      in
      E.app_var repr [ expr ]
    | Type.Constr (c, args) ->
      let f = str_of_id self c K_ty_of_cbor in
      (* FIXME: pass arguments *)
      E.app_var f [ expr ]
  in

  let ty = Type.chase_deep self.ty_defs ty in
  recurse ty expr

let cg_ty_decl_of_cbor (self : state) ~clique (ty_def : Type.def) :
    E.t * _ * E.t =
  let name = str_of_id self ty_def.name K_ty_of_cbor in

  let expr_self = E.var "self" in
  let args =
    List.map (fun tyv -> E.var @@ str_of_id self tyv K_var) ty_def.params
    @ [ E.cast expr_self (E.var "cbor") ]
  in

  let rhs =
    match ty_def.decl with
    | Type.Alias { target } -> cg_ty_of_cbor self ~clique target expr_self
    | Type.Record rows ->
      E.(
        match_ expr_self
        @@ vbar
             [
               (app_var "`Map" [ var "rows" ]
               -->
               let bs, fields =
                 List.map
                   (fun { Type.f; ty } ->
                     let f_name = str_of_id self f K_field in
                     let value =
                       let key = app_cstor "`Text" [ string_lit f_name ] in
                       let_ "v"
                         (app_var "Stdlib.List.assoc" [ key; var "rows" ])
                       @@ cg_ty_of_cbor ~clique self ty (var "v")
                     in

                     ((var f_name, value), (f_name, var f_name)))
                   rows
                 |> List.split
               in
               let_l bs @@ record fields);
               raw "_" --> raw {|cbor_error self "expected record"|};
             ])
    | Type.Algebraic cstors ->
      let conv_cstor { Type.c; args; labels } =
        let c = str_of_id self c K_cstor in
        let cbor_c_as_text = E.app_cstor "`Text" [ E.string_lit c ] in
        match (args, labels) with
        | [], _ -> E.(cbor_c_as_text --> var c)
        | _, None ->
          let vars = List.mapi (fun i _ -> E.var_f "_x_%d" i) args in
          let cbor_args =
            [ cbor_c_as_text; E.app_cstor "`Array" [ E.list_ vars ] ]
          in
          let rhs =
            E.app_cstor c
            @@ List.mapi
                 (fun i ty -> cg_ty_of_cbor self ~clique ty (E.var_f "_x_%d" i))
                 args
          in
          E.(app_cstor "`Array" [ E.list_ cbor_args ] --> rhs)
        | _, Some lbls ->
          (* TODO *)
          assert (List.length lbls = List.length args);
          let pat =
            E.app_cstor c
              [
                E.record
                @@ List.mapi
                     (fun i lbl ->
                       (str_of_id self lbl K_field, E.var_f "_x_%d" i))
                     lbls;
              ]
          in
          let cbor_args =
            [
              cbor_c_as_text;
              E.app_cstor "`Map"
              @@ List.mapi
                   (fun i ty ->
                     let lbl = List.nth lbls i in
                     E.tuple
                       [
                         E.app_cstor "`Text"
                           [ E.string_lit @@ str_of_id self lbl K_field ];
                         cg_ty_of_cbor self ~clique ty (E.var_f "_x_%d" i);
                       ])
                   args;
            ]
          in
          E.(pat --> app_cstor "`Array" [ E.list_ cbor_args ])
      in
      let else_case =
        E.(
          raw "_"
          --> app_var "cbor_error" [ expr_self; string_lit "expected sum type" ])
      in
      let cases = List.map conv_cstor cstors @ [ else_case ] in
      E.match_ expr_self @@ E.vbar cases
    | _ -> E.raw " assert false (* TODO *)"
    (* TODO *)
    (*
    | Type.Other | Type.Skolem ->
      let code = E.comment "(other)" @@ E.raw (String.capitalize_ascii name) in
      (code, false)
      *)
  in

  let ty_name = str_of_id self ty_def.name K_ty in
  let ret =
    if ty_def.params = [] then
      E.var ty_name
    else
      E.ty_app ty_name [ E.raw "_" ]
  in

  (E.app_var name args, Some ret, rhs)

let cg_fun (self : state) (f : Term.fun_decl) : E.t * _ * E.t =
  (cg_pat self f.pat, None, cg_term self f.body)

let rec cg_decl (self : state) (d : Decl.t) : unit =
  match d.view with
  | Decl.Ty { tys = defs } ->
    let clique = List.map (fun d -> d.Type.name) defs in
    (* declare types *)
    let tys = List.map (cg_ty_decl self ~clique) defs in
    add_decl self (A.Decl.ty_l tys);

    (* to_cbor *)
    let to_cbor = List.map (cg_ty_decl_to_cbor self ~clique) defs in
    add_decl self (A.Decl.let_l ~rec_:true to_cbor);

    (* of_cbor *)
    let of_cbor = List.map (cg_ty_decl_of_cbor self ~clique) defs in
    add_decl self (A.Decl.let_l ~rec_:true of_cbor)
  | Decl.Fun { recursive; fs } ->
    let fs = List.map (cg_fun self) fs in
    add_decl self (A.Decl.let_l ~rec_:recursive fs)
  | Decl.Module_alias (name, m) ->
    add_decl self
      (A.Decl.raw
      @@ spf "module %s = %s"
           (str_of_id self name K_mod)
           (str_of_id self m K_mod))
  | Decl.Module { name; items } ->
    (* enter module *)
    let@ () = with_module self ~mod_name:(str_of_id self name K_mod) in
    List.iter (cg_decl self) items

let codegen (decls : Decl.t list) : string =
  let ty_defs = Decl.ty_defs_of_decls decls in
  let st =
    {
      code = { decls = []; parent = None };
      seen = Str_tbl.create 8;
      ty_defs;
      cstor_labels = Uid.Tbl.create 16;
      uids = Uid_kind_tbl.create 32;
      gen = 1;
    }
  in
  add_decl st
    (A.Decl.raw (spf "(* generated from imandra-ast *)\n%s\n" prelude));
  List.iter (cg_decl st) decls;
  Fmt.asprintf "@[<v>%a@]@."
    Fmt.(list ~sep:(return "@ @ ") A.Decl.pp)
    (List.rev st.code.decls)
