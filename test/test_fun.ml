open Test_util

let%expect_test "test_fun.beam" =
  print_ast "test_fun.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile 1 test_fun.erl 1)
          (AttrMod 1 test_fun)
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
