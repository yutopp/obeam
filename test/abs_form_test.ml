(*
 * Copyright yutopp 2017 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

(* checker utils *)
let load_ast_from_beam beam_filename =
  let open Obeam in
  let beam_buf = Bitstring.bitstring_of_file beam_filename in
  match Beam.parse_layout beam_buf with
  | Ok (layout, _) ->
     let {
       Beam.cl_abst = opt_abst;
       Beam.cl_dbgi = opt_dbgi;
     } = layout in
     let parsed_result =
       match opt_abst with
       | Some abst ->
          External_term_format.parse abst.Beam.abst_buf
       | None ->
          begin
            match opt_dbgi with
            | Some dbgi ->
               External_term_format.parse dbgi.Beam.dbgi_buf
            | None ->
               failwith "abst and dbgi chunk is not found"
          end
     in
       (match parsed_result with
        | Ok (etf, _rest) ->
           Abstract_format.of_etf etf
        | Error (msg, _rest) ->
           failwith (Printf.sprintf "Failed: %s" msg))
  | Error (msg, _rest) ->
     failwith (Printf.sprintf "Failed : %s\n" msg)

let print_ast beam_filename =
  let open Obeam in
  load_ast_from_beam beam_filename
  |> [%sexp_of: (Abstract_format.t, Abstract_format.err_t) Result.t]
  |> Expect_test_helpers_kernel.print_s

(* test cases *)
let%expect_test "test01.beam" =
    print_ast "test01.beam";
    [%expect {|
      (Ok (
        AbstractCode (
          ModDecl (
            (AttrFile 1 test01.erl 1)
            (AttrMod 1 test01)
            (AttrExport 3 (
              (g 0)
              (h 1)))
            (DeclFun
             5
             f
             0
             ((
               ClsFun 5
               ()
               ()
               (ExprBody ((ExprLit (LitInteger 6 0)))))))
            (DeclFun
             8
             g
             0
             ((
               ClsFun 8
               ()
               ()
               (ExprBody ((ExprLit (LitInteger 9 10)))))))
            (SpecFun 11
              ()
              h
              1
              ((
                TyFun 11
                (TyProduct 11 ((TyPredef 11 integer ())))
                (TyPredef 11 string ()))))
            (DeclFun
             12
             h
             1
             ((
               ClsFun 12
               ((PatUniversal 12))
               ()
               (ExprBody ((ExprLit (LitString 13 abcdefg)))))))
            FormEof)))) |}]

let%expect_test "test02.beam" =
    print_ast "test02.beam";
    [%expect {|
      (Ok (
        AbstractCode (
          ModDecl (
            (AttrFile 1 test02.erl 1)
            (AttrMod 1 test02)
            (SpecFun 3
              ()
              i
              1
              ((
                TyContFun 3
                (TyPredef 3 fun (
                  (TyProduct 3 ((TyVar 3 A))) (TyPredef 3 integer ())))
                (TyCont ((
                  TyContRel 3
                  (TyContIsSubType 3)
                  (TyVar 3 A)
                  (TyPredef 3 integer ())))))))
            (DeclFun
             4
             i
             1
             ((ClsFun 4 ((PatVar 4 N)) () (ExprBody ((ExprVar 4 N))))))
            (SpecFun 6
              ()
              j
              1
              ((
                TyContFun 6
                (TyPredef 6 fun ((TyProduct 6 ((TyVar 6 A))) (TyVar 6 B)))
                (TyCont (
                  (TyContRel 6
                    (TyContIsSubType 6)
                    (TyVar 6 A)
                    (TyPredef 6 integer ()))
                  (TyContRel 7
                    (TyContIsSubType 7)
                    (TyVar 7 B)
                    (TyPredef 7 integer ())))))))
            (DeclFun
             8
             j
             1
             ((
               ClsFun 8
               ((PatVar 8 N))
               ()
               (ExprBody ((
                 ExprBinOp 9 *
                 (ExprVar 9 N)
                 (ExprVar 9 N)))))))
            FormEof)))) |}]

let%expect_test "test03.beam" =
    print_ast "test03.beam";
    [%expect {|
      (Ok (
        AbstractCode (
          ModDecl (
            (AttrFile 1 test03.erl 1)
            (AttrMod 1 test03)
            (DeclFun
             3
             f
             1
             ((ClsFun 3
                ((PatVar 3 N))
                ((
                  GuardSeq ((
                    Guard ((
                      GuardTestCall 3 (LitAtom 3 is_integer) ((GuardTestVar 3 N))))))))
                (ExprBody ((ExprLit (LitInteger 4 0)))))
              (ClsFun 5
                ((PatUniversal 5))
                ()
                (ExprBody ((ExprLit (LitInteger 6 2)))))))
            (DeclFun
             8
             g
             1
             ((
               ClsFun 8
               ((PatVar 8 R))
               ()
               (ExprBody ((
                 ExprCase 9
                 (ExprVar 9 R)
                 ((ClsCase 10
                    (PatLit (LitAtom 10 ok))
                    ()
                    (ExprBody ((ExprLit (LitInteger 10 1)))))
                  (ClsCase 11
                    (PatLit (LitAtom 11 error))
                    ()
                    (ExprBody ((ExprLit (LitInteger 11 2))))))))))))
            FormEof)))) |}]

let%expect_test "test04.beam" =
    print_ast "test04.beam";
    [%expect {|
      (Ok (
        AbstractCode (
          ModDecl (
            (AttrFile 1 test04.erl 1)
            (AttrMod 1 test04)
            (AttrExport 3 (
              (f 0)
              (g 1)
              (h 1)))
            (AttrExportType 4 (
              (tuple 2)
              (int   0)))
            (DeclType 6 tuple
              ((6 A)
               (6 B))
              (TyPredef 6 tuple (
                (TyVar 6 A)
                (TyVar 6 B))))
            (DeclOpaqueType 7 int () (TyPredef 7 integer ()))
            (SpecFun 9
              ()
              f
              0
              ((
                TyFun 9
                (TyProduct 9 ())
                (TyMap 9 (
                  (TyAssocExact 9 (TyLit (LitAtom 9 a)) (TyPredef 9 integer ()))
                  (TyAssoc 9 (TyLit (LitAtom 9 b)) (TyPredef 9 integer ())))))))
            (DeclFun
             10
             f
             0
             ((
               ClsFun 10
               ()
               ()
               (ExprBody ((
                 ExprMapUpdate 10
                 (ExprMapCreation 10 (
                   (ExprAssoc 10
                     (ExprLit (LitAtom    10 a))
                     (ExprLit (LitInteger 10 1)))
                   (ExprAssoc 10
                     (ExprLit (LitAtom    10 b))
                     (ExprLit (LitInteger 10 2)))))
                 ((ExprAssocExact 10
                    (ExprLit (LitAtom    10 a))
                    (ExprLit (LitInteger 10 42)))
                  (ExprAssoc 10
                    (ExprLit (LitAtom    10 c))
                    (ExprLit (LitInteger 10 3))))))))))
            (SpecFun 12
              ()
              g
              1
              ((
                TyFun 12
                (TyProduct 12 ((
                  TyMap 12 ((TyAssocExact 12 (TyLit (LitAtom 12 a)) (TyVar 12 A))))))
                (TyVar 12 A))))
            (DeclFun
             13
             g
             1
             ((
               ClsFun 13
               ((
                 PatMap 13 ((PatAssocExact 13 (PatLit (LitAtom 13 a)) (PatVar 13 N)))))
               ()
               (ExprBody ((ExprVar 13 N))))))
            (SpecFun 15
              ()
              h
              1
              ((TyFun 15 (TyProduct 15 ((TyAnyMap 15))) (TyAnyMap 15))))
            (DeclFun
             16
             h
             1
             ((
               ClsFun 16
               ((PatVar 16 M))
               ((
                 GuardSeq ((
                   Guard ((
                     GuardTestBinOp 16 andalso
                     (GuardTestBinOp 16 =:=
                       (GuardTestVar 16 M)
                       (GuardTestMapCreation 16 ((
                         GuardTestAssoc 16
                         (GuardTestLit (LitAtom    16 a))
                         (GuardTestLit (LitInteger 16 42))))))
                     (GuardTestBinOp 16 =:=
                       (GuardTestMapUpdate 16
                         (GuardTestVar 16 M)
                         ((
                           GuardTestAssocExact 16
                           (GuardTestLit (LitAtom    16 a))
                           (GuardTestLit (LitInteger 16 0)))))
                       (GuardTestMapCreation 16 ((
                         GuardTestAssoc 16
                         (GuardTestLit (LitAtom    16 a))
                         (GuardTestLit (LitInteger 16 0))))))))))))
               (ExprBody ((ExprVar 16 M))))))
            FormEof)))) |}]

let%expect_test "test05.beam" =
    print_ast "test05.beam";
    [%expect {|
      (Ok (
        AbstractCode (
          ModDecl (
            (AttrFile 1 test05.erl 1)
            (AttrMod 1 test05)
            (DeclRecord 3 (
              (3 a
                ()
                ())
              (4 b ((ExprLit (LitInteger 4 42))) ())
              (5 c () ((TyPredef 5 string ())))
              (6 d ((ExprLit (LitInteger 6 57))) ((TyPredef 6 integer ())))))
            FormEof)))) |}]

let%expect_test "test06.beam" =
    print_ast "test06.beam";
    [%expect {|
      (Ok (
        AbstractCode (
          ModDecl (
            (AttrFile 1 test06.erl 1)
            (AttrMod 1 test06)
            (AttrWild 3 compile (List ((Atom export_all))))
            (AttrWild 5 vsn (Atom 1.0.0))
            (AttrWild 7 on_load (
              Tuple 2 (
                (Atom    f)
                (Integer 0))))
            (AttrWild 9  behaviour (Atom   gen_server))
            (AttrWild 11 foo       (Binary bar))
            (DeclFun
             13
             f
             0
             ((
               ClsFun 13
               ()
               ()
               (ExprBody ((ExprLit (LitAtom 13 ok)))))))
            FormEof)))) |}]

let%expect_test "test07.beam" =
    print_ast "test07.beam";
    [%expect {|
      (Ok (
        AbstractCode (
          ModDecl (
            (AttrFile 1 test07.erl 1)
            (AttrMod 1 test07)
            (AttrExport 3 (
              (s 0)
              (l 0)))
            (DeclFun
             5
             s
             0
             ((
               ClsFun 5
               ()
               ()
               (ExprBody ((
                 ExprLit (
                   LitBigInt
                   6
                   123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890)))))))
            (DeclFun
             8
             l
             0
             ((
               ClsFun 8
               ()
               ()
               (ExprBody ((
                 ExprLit (
                   LitBigInt
                   9
                   123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890)))))))
            FormEof)))) |}]

let%expect_test "test08.beam" =
    print_ast "test08.beam";
    [%expect {|
      (Ok (
        AbstractCode (
          ModDecl (
            (AttrFile 1 test08.erl 1)
            (AttrMod 1 test08)
            (AttrExport 3 (
              (f 0)
              (f 1)
              (f 2)))
            (DeclFun
             5
             f
             0
             ((
               ClsFun 5
               ()
               ()
               (ExprBody (
                 (ExprMatch 6 (PatVar 6 F) (ExprLit (LitAtom 6 f)))
                 (ExprLocalCall 8
                   (ExprLit (LitAtom 8 f))
                   ((ExprLit (LitInteger 8 1))))
                 (ExprLocalCall 9 (ExprVar 9 F) ((ExprLit (LitInteger 9 1))))
                 (ExprLocalCall 10
                   (ExprCase 10
                     (ExprLit (LitAtom 10 true))
                     ((ClsCase 10 (PatUniversal 10) () (ExprBody ((ExprVar 10 F))))))
                   ((ExprLit (LitInteger 10 1)))))))))
            (DeclFun
             12
             f
             1
             ((
               ClsFun 12
               ((PatVar 12 N))
               ()
               (ExprBody (
                 (ExprMatch 13 (PatVar 13 Test08) (ExprLit (LitAtom 13 test08)))
                 (ExprMatch 14 (PatVar 14 F) (ExprLit (LitAtom 14 f)))
                 (ExprRemoteCall 16 16
                   (ExprLit (LitAtom 16 test08))
                   (ExprLit (LitAtom 16 f))
                   ((ExprVar 16 N) (ExprLit (LitInteger 16 2))))
                 (ExprRemoteCall 17 17
                   (ExprVar 17 Test08)
                   (ExprVar 17 F)
                   ((ExprVar 17 N) (ExprLit (LitInteger 17 2))))
                 (ExprRemoteCall 18 18
                   (ExprCase 18
                     (ExprLit (LitAtom 18 true))
                     ((
                       ClsCase 18
                       (PatUniversal 18)
                       ()
                       (ExprBody ((ExprVar 18 Test08))))))
                   (ExprCase 18
                     (ExprLit (LitAtom 18 true))
                     ((ClsCase 18 (PatUniversal 18) () (ExprBody ((ExprVar 18 F))))))
                   ((ExprVar 18 N) (ExprLit (LitInteger 18 2)))))))))
            (DeclFun
             20
             f
             2
             ((
               ClsFun 20
               ((PatVar 20 N)
                (PatVar 20 M))
               ()
               (ExprBody ((
                 ExprBinOp 20 +
                 (ExprVar 20 N)
                 (ExprVar 20 M)))))))
            (DeclFun
             22
             g
             0
             ((
               ClsFun 22
               ()
               ()
               (ExprBody (
                 (ExprMatch 23 (PatVar 23 Test08) (ExprLit (LitAtom 23 test08)))
                 (ExprMatch 24 (PatVar 24 F) (ExprLit (LitAtom 24 f)))
                 (ExprMatch 25 (PatVar 25 Zero) (ExprLit (LitInteger 25 0)))
                 (ExprLocalFunRef 27 f 0)
                 (ExprRemoteFunRef 29
                   (AtomVarAtom       29 test08)
                   (AtomVarAtom       29 f)
                   (IntegerVarInteger 29 0))
                 (ExprRemoteFunRef 30
                   (AtomVarVar    30 Test08)
                   (AtomVarVar    30 F)
                   (IntegerVarVar 30 Zero))
                 (ExprFun 32
                   ()
                   ((ClsFun 32
                      ((PatLit (LitInteger 32 42)))
                      ()
                      (ExprBody ((ExprLit (LitInteger 32 42)))))
                    (ClsFun 33
                      ((PatUniversal 33))
                      ()
                      (ExprBody ((ExprLit (LitInteger 33 57)))))))
                 (ExprFun 34
                   (F)
                   ((ClsFun 34
                      ((PatLit (LitInteger 34 42)))
                      ()
                      (ExprBody ((ExprLit (LitInteger 34 42)))))
                    (ClsFun 35
                      ((PatUniversal 35))
                      ()
                      (ExprBody ((ExprLit (LitInteger 35 57))))))))))))
            FormEof)))) |}]

let%expect_test "test09.beam" =
    print_ast "test09.beam";
    [%expect {|
      (Ok (
        AbstractCode (
          ModDecl (
            (AttrFile 1 test09.erl 1)
            (AttrMod 1 test09)
            (AttrExportType 3 ((b 0)))
            (DeclType 5 a () (TyPredef 5 term ()))
            (DeclType 6 b () (TyUser   6 a    ()))
            FormEof)))) |}]
