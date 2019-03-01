open Test_util

let%expect_test "test_float.beam" =
  print_ast "test_float.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_float.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_float))
          (AttrExport (line 3) (function_arity_list ((f 0))))
          (DeclFun
            (line          6)
            (function_name f)
            (arity         0)
            (clauses ((
              ClsFun
              (line 6)
              (patterns       ())
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprLit (
                      lit (
                        LitFloat
                        (line  6)
                        (float 0.1))))))))))))
          FormEof)))) |}]
