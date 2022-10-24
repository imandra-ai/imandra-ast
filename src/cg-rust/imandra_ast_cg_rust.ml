module Str_tbl = CCHashtbl.Make (CCString)

let spf = Printf.sprintf
let bpf = Printf.bprintf

type state = {
  out: Buffer.t;
  ty_defs: Type.Defs.t;
  cstor_labels: Uid.t list Uid.Tbl.t; (* cstor -> its labels *)
  uids: string Uid.Tbl.t;
  seen: unit Str_tbl.t;
  mutable gen: int;
}

type kind =
  | K_ty_cstor
  | K_ty_var
  | K_cstor
  | K_field
  | K_fun
  | K_ty
  | K_var

let prelude =
  {rust|
use num_bigint::{BigInt as BigInt, Zero, One, Add};
use num_rational::BigRational as Real;
|rust}

let gensym (self : state) (id : Uid.t) (base : string) : string =
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

(** find or create rust symbol for this ID *)
let str_of_id (self : state) (id : Uid.t) (kind : kind) : string =
  Uid.Tbl.get_or_add self.uids ~k:id ~f:(fun id ->
      (* FIXME: escape rust keywords *)
      let base =
        match kind with
        | K_ty_cstor | K_ty | K_ty_var | K_cstor ->
          String.capitalize_ascii (Uid.name id)
        | K_var | K_fun | K_field -> String.uncapitalize_ascii (Uid.name id)
      in

      let s = gensym self id base in
      Str_tbl.add self.seen s ();
      Uid.Tbl.add self.uids id s;
      s)

let str_of_cg (self : state) (cg : state -> Buffer.t -> 'a -> unit) (x : 'a) :
    string =
  let buf = Buffer.create 32 in
  cg self buf x;
  Buffer.contents buf

let cg_ty (self : state) (out : Buffer.t) (ty : Type.t) : unit =
  let rec recurse out ty =
    match Type.view ty with
    | Type.Var v ->
      let str = str_of_id self v K_var in
      bpf out "%s" str
    | Type.Arrow (_, _, _) ->
      let args, ret = Type.in_out_types self.ty_defs ty in
      bpf out "(fn(";
      List.iteri
        (fun i a ->
          if i > 0 then bpf out ",";
          recurse out a)
        args;
      bpf out ") -> ";
      recurse out ret;
      bpf out ")"
    | Type.Tuple l ->
      bpf out "(fn(";
      List.iteri
        (fun i a ->
          if i > 0 then bpf out ",";
          recurse out a)
        l;
      bpf out ")"
    | Type.Constr (c, []) ->
      let repr =
        match Uid.name c with
        | "int" -> "BigInt"
        | "real" -> "Real"
        | _name -> str_of_id self c K_cstor ^ spf " /* %s */" _name
      in

      bpf out "%s" repr
    | Type.Constr (c, args) ->
      (* FIXME: see if [c] takes a record *)
      bpf out "%s(" (str_of_id self c K_cstor);
      List.iter (fun a -> bpf out "%a," recurse a) args;
      bpf out ")"
  in

  let ty = Type.chase_deep self.ty_defs ty in
  recurse out ty

let cg_ty_decl (self : state) (out : Buffer.t) (ty_def : Type.def) : unit =
  let name = str_of_id self ty_def.name K_ty in
  let args =
    match ty_def.params with
    | [] -> ""
    | l ->
      spf "<%s>" @@ String.concat ","
      @@ List.map (fun tyv -> str_of_id self tyv K_ty_var) l
  in

  (match ty_def.decl with
  | Type.Record rows ->
    bpf out "pub struct %s%s {\n" name args;
    List.iter
      (fun { Type.f; ty } ->
        bpf out "  pub %s: %a,\n" (str_of_id self f K_field) (cg_ty self) ty)
      rows;

    bpf out "}"
  | Type.Algebraic cstors ->
    bpf out "pub enum %s%s {\n" name args;
    List.iter
      (fun { Type.c; args; labels } ->
        bpf out "  %s" (str_of_id self c K_cstor);
        (match (args, labels) with
        | [], _ -> ()
        | _, None ->
          bpf out "(";
          List.iteri
            (fun i a ->
              if i > 0 then bpf out ",";
              cg_ty self out a)
            args;
          bpf out ")"
        | _, Some lbls ->
          assert (List.length lbls = List.length args);
          Uid.Tbl.add self.cstor_labels c lbls;
          bpf out "{\n";
          List.iter2
            (fun lbl a ->
              bpf out "%s: %a," (str_of_id self lbl K_field) (cg_ty self) a)
            lbls args;
          bpf out "}");
        bpf out ",\n")
      cstors;
    bpf out "}"
  | Type.Builtin _ -> assert false (* TODO *)
  | Type.Alias { target } ->
    bpf out "pub type %s%s = " name args;
    cg_ty self out target
  | Type.Other | Type.Skolem -> bpf out "// (other)\npub struct %s%s;" name args);
  bpf out "\n\n"

let cg_fun (self : state) (out : Buffer.t) (f : Term.fun_decl) : unit =
  bpf self.out "// TODO: fun %s\n" (str_of_id self f.name K_fun);
  () (* TODO *)

let cg_decl (self : state) (d : Decl.t) : unit =
  match d.view with
  | Decl.Ty defs -> List.iter (cg_ty_decl self self.out) defs
  | Decl.Fun { recursive = _; fs } -> List.iter (cg_fun self self.out) fs
  | Decl.Module_alias _ | Decl.Module _ ->
    bpf self.out "// TODO: module\n";
    () (* TODO *)

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
  bpf st.out "// generated from imandra-ast\n%s\n" prelude;
  List.iter (cg_decl st) decls;
  Buffer.contents st.out
