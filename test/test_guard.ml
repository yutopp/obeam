open Test_util

let%expect_test "test_guard.beam" =
  print_ast "test_guard.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile 1 test_guard.erl 1)
          (AttrMod 1 test_guard)
          (AttrExport 3 ((f 1)))
          (DeclFun
           5
           f
           1
           ((ClsFun 5
              ((PatVar 5 N))
              ((
                GuardSeq ((
                  Guard ((
                    GuardTestCall 5 (LitAtom 5 is_integer) ((GuardTestVar 5 N))))))))
              (ExprBody ((ExprLit (LitInteger 6 0)))))
            (ClsFun 7
              ((PatUniversal 7))
              ()
              (ExprBody ((ExprLit (LitInteger 8 2)))))))
          FormEof)))) |}]
