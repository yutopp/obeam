open Test_util

let%expect_test "test_receive.beam" =
  print_ast "test_receive.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_receive.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_receive))
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
                    ExprReceive
                    (line 7)
                    (clauses (
                      (ClsCase
                        (line 8)
                        (pattern (
                          PatLit (
                            lit (
                              LitAtom
                              (line 8)
                              (atom ok)))))
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprLit (
                                lit (
                                  LitAtom
                                  (line 8)
                                  (atom ok)))))))))
                      (ClsCase
                        (line 9)
                        (pattern (
                          PatLit (
                            lit (
                              LitAtom
                              (line 9)
                              (atom error)))))
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprLit (
                                lit (
                                  LitAtom
                                  (line 9)
                                  (atom error))))))))))))))))))))
          FormEof)))) |}]
