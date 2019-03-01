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
            (line 11)
            (fields (
              (RecordField
                (line       11)
                (field_name name)
                (ty ((
                  TyPredef
                  (line 11)
                  (name string)
                  (args ()))))
                (default_expr ()))
              (RecordField
                (line       11)
                (field_name param)
                (ty ((
                  TyPredef
                  (line 11)
                  (name term)
                  (args ()))))
                (default_expr ())))))
          (DeclType
            (line 12)
            (name record_as_type)
            (tvars ((12 A)))
            (ty (
              TyRecord
              (line             12)
              (name             state)
              (line_field_types 12)
              (field_types ((
                RecordFieldType
                (line      12)
                (line_name 12)
                (name      param)
                (ty (
                  TyVar
                  (line 12)
                  (id   A)))))))))
          FormEof)))) |}]
