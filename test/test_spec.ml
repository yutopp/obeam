open Test_util

let%expect_test "test_spec.beam" =
  print_ast "test_spec.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile 1 test_spec.erl 1)
          (AttrMod 1 test_spec)
          (AttrExport 3 (
            (f 1)
            (g 1)))
          (SpecFun 5
            ()
            f
            1
            ((
              TyContFun 5
              (TyPredef 5 fun (
                (TyProduct 5 ((TyVar 5 A))) (TyPredef 5 integer ())))
              (TyCont ((
                TyContRel 5
                (TyContIsSubType 5)
                (TyVar 5 A)
                (TyPredef 5 integer ())))))))
          (DeclFun
           6
           f
           1
           ((ClsFun 6 ((PatVar 6 N)) () (ExprBody ((ExprVar 6 N))))))
          (SpecFun 8
            ()
            g
            1
            ((
              TyContFun 8
              (TyPredef 8 fun ((TyProduct 8 ((TyVar 8 A))) (TyVar 8 B)))
              (TyCont (
                (TyContRel 8
                  (TyContIsSubType 8)
                  (TyVar 8 A)
                  (TyPredef 8 integer ()))
                (TyContRel 9
                  (TyContIsSubType 9)
                  (TyVar 9 B)
                  (TyPredef 9 integer ())))))))
          (DeclFun
           10
           g
           1
           ((
             ClsFun 10
             ((PatVar 10 N))
             ()
             (ExprBody ((
               ExprBinOp 11 *
               (ExprVar 11 N)
               (ExprVar 11 N)))))))
          FormEof)))) |}]
