open Test_util

let%expect_test "test_record.beam" =
  print_ast "test_record.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_record.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_record))
          (DeclRecord
            (line 4)
            (fields (
              (RecordField
                (line       4)
                (field_name a)
                (ty           ())
                (default_expr ()))
              (RecordField
                (line       5)
                (field_name b)
                (ty ())
                (default_expr ((
                  ExprLit (
                    lit (
                      LitInteger
                      (line    5)
                      (integer 42)))))))
              (RecordField
                (line       6)
                (field_name c)
                (ty ((
                  TyPredef
                  (line 6)
                  (name string)
                  (args ()))))
                (default_expr ()))
              (RecordField
                (line       7)
                (field_name d)
                (ty ((
                  TyPredef
                  (line 7)
                  (name integer)
                  (args ()))))
                (default_expr ((
                  ExprLit (
                    lit (
                      LitInteger
                      (line    7)
                      (integer 57))))))))))
          (AttrExport (line 9) (function_arity_list ((f 0))))
          (DeclFun
            (line          11)
            (function_name f)
            (arity         0)
            (clauses ((
              ClsFun
              (line 11)
              (patterns       ())
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs (
                    (ExprMatch
                      (line 12)
                      (pattern (
                        PatVar
                        (line 12)
                        (id   R)))
                      (body (
                        ExprRecord
                        (line 12)
                        (name r)
                        (record_fields (
                          (12 a (
                            ExprLit (
                              lit (
                                LitInteger
                                (line    12)
                                (integer 3)))))
                          (12 c (
                            ExprLit (
                              lit (
                                LitString
                                (line 12)
                                (str  hello))))))))))
                    (ExprMatch
                      (line 13)
                      (pattern (PatUniversal (line 13)))
                      (body (
                        ExprRecordFieldAccess
                        (line 13)
                        (expr (
                          ExprVar
                          (line 13)
                          (id   R)))
                        (name       r)
                        (field_name c))))
                    (ExprMatch
                      (line 14)
                      (pattern (
                        PatVar
                        (line 14)
                        (id   Index)))
                      (body (
                        ExprRecordFieldIndex
                        (line       14)
                        (name       r)
                        (field_name b))))
                    (ExprRecordUpdate
                      (line 15)
                      (expr (
                        ExprVar
                        (line 15)
                        (id   R)))
                      (name r)
                      (update_fields (
                        (15 a (
                          ExprLit (
                            lit (
                              LitInteger
                              (line    15)
                              (integer 100)))))
                        (15 c (
                          ExprLit (
                            lit (
                              LitString
                              (line 15)
                              (str  hoge))))))))))))))))
          FormEof)))) |}]
