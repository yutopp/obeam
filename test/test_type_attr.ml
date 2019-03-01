open Test_util

let%expect_test "test_type_attr.beam" =
  print_ast "test_type_attr.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_type_attr.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_type_attr))
          (AttrExportType
            (line 3)
            (type_arity_list (
              (tuple 2)
              (int   0))))
          (DeclType
            (line 6)
            (name tuple)
            (tvars (
              (6 A)
              (6 B)))
            (ty (
              TyTuple
              (line 6)
              (elements (
                (TyVar (line 6) (id A))
                (TyVar (line 6) (id B)))))))
          (DeclOpaqueType
            (line 9)
            (name int)
            (tvars ())
            (ty (
              TyPredef
              (line 9)
              (name integer)
              (args ()))))
          (DeclRecord
            (line 12)
            (fields (
              (RecordField
                (line       12)
                (field_name name)
                (ty ((
                  TyPredef
                  (line 12)
                  (name string)
                  (args ()))))
                (default_expr ()))
              (RecordField
                (line       12)
                (field_name param)
                (ty ((
                  TyPredef
                  (line 12)
                  (name term)
                  (args ()))))
                (default_expr ())))))
          (DeclType
            (line 13)
            (name record_as_type)
            (tvars ((13 A)))
            (ty (
              TyRecord
              (line      13)
              (line_name 13)
              (name      state)
              (field_types ((
                RecordFieldType
                (line      13)
                (line_name 13)
                (name      param)
                (ty (
                  TyVar
                  (line 13)
                  (id   A)))))))))
          FormEof)))) |}]
