open Test_util

let%expect_test "test_char.beam" =
  print_ast "test_char.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile 1 test_char.erl 1)
          (AttrMod 1 test_char)
          (AttrExport 3 (
            (f 0)
            (g 0)))
          (DeclFun
           6
           f
           0
           ((
             ClsFun 6
             ()
             ()
             (ExprBody ((ExprLit (LitChar 6 U+0061)))))))
          (DeclFun
           9
           g
           0
           ((
             ClsFun 9
             ()
             ()
             (ExprBody ((ExprLit (LitChar 9 U+3042)))))))
          FormEof)))) |}]
