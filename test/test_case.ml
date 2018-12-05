open Test_util

let%expect_test "test_case.beam" =
  print_ast "test_case.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile 1 test_case.erl 1)
          (AttrMod 1 test_case)
          (AttrExport 3 ((f 1)))
          (DeclFun
           5
           f
           1
           ((
             ClsFun 5
             ((PatVar 5 R))
             ()
             (ExprBody ((
               ExprCase 6
               (ExprVar 6 R)
               ((ClsCase 7
                  (PatLit (LitAtom 7 ok))
                  ()
                  (ExprBody ((ExprLit (LitInteger 7 1)))))
                (ClsCase 8
                  (PatLit (LitAtom 8 error))
                  ()
                  (ExprBody ((ExprLit (LitInteger 8 2))))))))))))
          FormEof)))) |}]
