open Test_util

let%expect_test "test_op_pattern.beam" =
  print_ast "test_op_pattern.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_op_pattern.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_op_pattern))
          (AttrExport (line 3) (function_arity_list ((f 1))))
          (DeclFun
            (line          6)
            (function_name f)
            (arity         1)
            (clauses (
              (ClsFun
                (line 6)
                (patterns ((
                  PatBinOp
                  (line 6)
                  (op   ++)
                  (lhs (
                    PatLit (
                      lit (
                        LitString
                        (line 6)
                        (str  abc)))))
                  (rhs (
                    PatVar
                    (line 6)
                    (id   T))))))
                (guard_sequence ())
                (body (
                  ExprBody (
                    exprs ((
                      ExprVar
                      (line 6)
                      (id   T)))))))
              (ClsFun
                (line 7)
                (patterns ((
                  PatBinOp
                  (line 7)
                  (op   +)
                  (lhs (
                    PatLit (
                      lit (
                        LitInteger
                        (line    7)
                        (integer 1)))))
                  (rhs (
                    PatLit (
                      lit (
                        LitInteger
                        (line    7)
                        (integer 2))))))))
                (guard_sequence ())
                (body (
                  ExprBody (
                    exprs ((
                      ExprLit (
                        lit (
                          LitInteger
                          (line    7)
                          (integer 3))))))))))))
          FormEof)))) |}]
