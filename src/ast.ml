(*
 * Copyright yutopp 2017 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

module SimpleForm =
  struct
    type t =
      | Integer of int
      | Atom of string
      | Tuple of int * t list
      | String of string
      | List of t list
      | Float of float
    [@@deriving show]

    (* from External term format *)
    let rec of_etf etf =
      let module ETF = External_term in
      match etf with
      | ETF.SmallInteger v ->
         Integer v
      | ETF.Atom name ->
         Atom name
      | ETF.SmallTuple (n, xs) ->
         Tuple (n, xs |> List.map of_etf)
      | ETF.Nil ->
         List []
      | ETF.String v ->
         String v
      | ETF.List (xs, ETF.Nil) ->
         List (xs |> List.map of_etf)
      | ETF.List (xs, tail) ->
         (* TODO: fix *)
         failwith "unsupported"
      | ETF.NewFloat v ->
         Float v
  end

let raise_unknown_error form sf =
  failwith (Printf.sprintf "%s: unknown / %s"
                           form
                           (SimpleForm.show sf))

(* http://erlang.org/doc/apps/erts/absform.html *)
type t =
  | AbstractCode of form_t

and form_t =
  | ModDecl of form_t list

  | AttrExport of int * (string * int) list
  | AttrImport of int * (string * int) list
  | AttrMod of int * string
  | AttrFile of int * string * int
  | DeclFun of int * string * int * clause_t list
  | SpecFun of int * string option * string * int * type_t list
  | DeclRecord
  | DeclType
  | AttrWild

  | FormEof

and literal_t =
  | LitAtom of int * string
  | LitInteger of int * int
  | LitString of int * string

and pattern_t =
  | PatUniversal of int
  | PatVar of int * string

and expr_t =
  | ExprBody of expr_t list
  | ExprBinOp of int * string * expr_t * expr_t
  | ExprVar of int * string
  | ExprLit of literal_t

and clause_t =
  | ClsFun of int * pattern_t list * guard_t list * expr_t

and guard_t =
  unit

and type_t =
  | TyAnn of int * type_t * type_t
  | TyPredef of int * string * type_t list
  | TyProduct of int * type_t list
  | TyVar of int * string

  | TyContFun of int * type_t * type_t
  | TyFun of int * type_t * type_t

  | TyCont of type_t list
  | TyContRel of int * type_t * type_t * type_t
  | TyContIsSubType of int
[@@deriving show]

(*
 * Entry
 *)
let rec of_sf sf =
  let module Sf = SimpleForm in
  match sf with
  | Sf.Tuple (2, [Sf.Atom "raw_abstract_v1"; code]) ->
     AbstractCode (code |> form_of_sf)

  | _ ->
     raise_unknown_error "root" sf

(*
 * 7.1  Module Declarations and Forms
 *)
and form_of_sf sf =
  let module Sf = SimpleForm in
  match sf with
  (* module declaration *)
  | Sf.List forms ->
     ModDecl (forms |> List.map form_of_sf)

  (* attribute -export *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "export";
                  Sf.List fa_list
                 ]) ->
     AttrExport (line, fa_list |> List.map fa_list_of_sf)

  (* attribute -import *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "import";
                  Sf.List fa_list
                 ]) ->
     AttrImport (line, fa_list |> List.map fa_list_of_sf)

  (* attribute -module *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "module";
                  Sf.Atom module_name
                 ]) ->
     AttrMod (line, module_name)

  (* attribute -file *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "file";
                  Sf.Tuple (2, [Sf.String file; Sf.Integer file_line])
                 ]) ->
     AttrFile (line, file, file_line)

  (* function declaration *)
  | Sf.Tuple (5, [Sf.Atom "function";
                  Sf.Integer line;
                  Sf.Atom name;
                  Sf.Integer arity;
                  Sf.List f_clauses]) ->
     DeclFun (line, name, arity, f_clauses |> List.map cls_of_sf)


  (* function specification *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "spec";
                  Sf.Tuple (2, [Sf.Tuple (2, [Sf.Atom name; Sf.Integer arity]);
                                Sf.List specs])
                 ]) ->
     SpecFun (line, None, name, arity, specs |> List.map fun_type_of_sf)

  (* function specification(Mod) *)
  | Sf.Tuple (4, [Sf.Atom "attribute";
                  Sf.Integer line;
                  Sf.Atom "spec";
                  Sf.Tuple (2, [Sf.Tuple (3, [Sf.Atom m; Sf.Atom name; Sf.Integer arity]);
                                Sf.List specs])
                 ]) ->
     SpecFun (line, Some m, name, arity, specs |> List.map fun_type_of_sf)

  (* eof *)
  | Sf.Tuple (2, [Sf.Atom "eof"; Sf.Integer line]) ->
     FormEof

  | _ ->
     raise_unknown_error "form" sf

and fa_list_of_sf sf =
  let module Sf = SimpleForm in
  match sf with
  | Sf.Tuple (2, [Sf.Atom name; Sf.Integer arity]) ->
     (name, arity)

  | _ ->
     raise_unknown_error "fa_list" sf

(*
 * 7.2  Atomic Literals
 *)
and lit_of_sf sf =
  let module Sf = SimpleForm in
  match sf with
  | Sf.Tuple (3, [Sf.Atom "atom"; Sf.Integer line; Sf.Atom v]) ->
     LitAtom (line, v)

  | Sf.Tuple (3, [Sf.Atom "char"; Sf.Integer line; _]) ->
     failwith "TODO"

  | Sf.Tuple (3, [Sf.Atom "float"; Sf.Integer line; _]) ->
     failwith "TODO"

  | Sf.Tuple (3, [Sf.Atom "integer"; Sf.Integer line; Sf.Integer v]) ->
     LitInteger (line, v)

  | Sf.Tuple (3, [Sf.Atom "string"; Sf.Integer line; Sf.String v]) ->
     LitString (line, v)

  | _ ->
     raise_unknown_error "literal" sf

(*
 * 7.3  Patterns
 *)
and pat_of_sf sf =
  let module Sf = SimpleForm in
  match sf with
  (* a variable pattern *)
  | Sf.Tuple (3, [Sf.Atom "var"; Sf.Integer line; Sf.Atom "_"]) ->
     PatUniversal line

  (* a variable pattern *)
  | Sf.Tuple (3, [Sf.Atom "var"; Sf.Integer line; Sf.Atom id]) ->
     PatVar (line, id)

  | _ ->
     raise_unknown_error "pattern" sf

(*
 * 7.4  Expressions
 *)
and expr_of_sf sf =
  let module Sf = SimpleForm in
  match sf with
  | Sf.List es ->
     ExprBody (es |> List.map expr_of_sf)

  (* an operator expression binary *)
  | Sf.Tuple (5, [Sf.Atom "op";
                  Sf.Integer line;
                  Sf.Atom op;
                  p1;
                  p2]) ->
     ExprBinOp (line, op, p1 |> expr_of_sf, p2 |> expr_of_sf)

  (* a variable *)
  | Sf.Tuple (3, [Sf.Atom "var"; Sf.Integer line; Sf.Atom id]) ->
     ExprVar (line, id)

  (* atomic literal *)
  | v ->
     ExprLit (v |> lit_of_sf)

(*
 * 7.5  Clauses
 *)
and cls_of_sf sf =
  let module Sf = SimpleForm in
  match sf with
  (* function clause *)
  | Sf.Tuple (5, [
                 Sf.Atom "clause";
                 Sf.Integer line;
                 Sf.List patterns;
                 Sf.List [];
                 body
               ]) ->
     ClsFun (line, patterns |> List.map pat_of_sf, [], body |> expr_of_sf)

  | _ ->
     raise_unknown_error "cls" sf

(*
 * 7.7  Types
 *)
and type_of_sf sf =
  let module Sf = SimpleForm in
  match sf with
  (* annotated type *)
  | Sf.Tuple (3, [Sf.Atom "ann_type";
                  Sf.Integer line;
                  Sf.List [a; t]]) ->
     TyAnn (line, a |> type_of_sf, t |> type_of_sf)

  (* product type *)
  | Sf.Tuple (4, [Sf.Atom "type";
                  Sf.Integer line;
                  Sf.Atom "product";
                  Sf.List args]) ->
     TyProduct (line, args |> List.map type_of_sf)

  (* predefined (or built-in) type *)
  | Sf.Tuple (4, [Sf.Atom "type";
                  Sf.Integer line;
                  Sf.Atom n;
                  Sf.List args]) ->
     TyPredef (line, n, args |> List.map type_of_sf)

  (* type variable *)
  | Sf.Tuple (3, [Sf.Atom "var"; Sf.Integer line; Sf.Atom id]) ->
     TyVar (line, id)

  | _ ->
     raise_unknown_error "type" sf

and fun_type_of_sf sf =
  let module Sf = SimpleForm in
  match sf with
  (* constrained function type *)
  | Sf.Tuple (4, [Sf.Atom "type";
                  Sf.Integer line;
                  Sf.Atom "bounded_fun";
                  Sf.List [fun_ty; cont]
                 ]) ->
     TyContFun (line, fun_ty |> type_of_sf, cont |> cont_of_sf)

  (* function type *)
  | Sf.Tuple (4, [Sf.Atom "type";
                  Sf.Integer line;
                  Sf.Atom "fun";
                  Sf.List [params; ret]]) ->
     TyFun (line, params |> type_of_sf, ret |> type_of_sf)

  | _ ->
     raise_unknown_error "fun_type" sf

and cont_of_sf sf =
  let module Sf = SimpleForm in
  match sf with
  | Sf.List constraints ->
     TyCont (constraints |> List.map cont_of_sf)

  | Sf.Tuple (4, [Sf.Atom "type";
                  Sf.Integer line;
                  Sf.Atom "constraint";
                  Sf.List [c; Sf.List [v; t]]
                 ]) ->
     TyContRel (line, c |> cont_of_sf, v |> type_of_sf, t |> type_of_sf)

  | Sf.Tuple (3, [Sf.Atom "atom"; Sf.Integer line; Sf.Atom "is_subtype"]) ->
     TyContIsSubType line

  | _ ->
     raise_unknown_error "cont_type" sf

(**)
let of_etf etf =
  SimpleForm.of_etf etf |> of_sf
