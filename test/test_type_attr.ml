open Test_util

let%expect_test "test_type_attr.beam" =
  print_ast "test_type_attr.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile 1 test_type_attr.erl 1)
          (AttrMod 1 test_type_attr)
          (AttrExportType 3 (
            (tuple 2)
            (int   0)))
          (DeclType 5 tuple
            ((5 A)
             (5 B))
            (TyTuple 5 (
              (TyVar 5 A)
              (TyVar 5 B))))
          (DeclOpaqueType 6 int () (TyPredef 6 integer ()))
          FormEof)))) |}]
