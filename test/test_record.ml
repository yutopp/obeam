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
          FormEof)))) |}]
