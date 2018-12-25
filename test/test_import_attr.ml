open Test_util

let%expect_test "test_import_attr.beam" =
  print_ast "test_import_attr.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile 1 test_import_attr.erl 1)
          (AttrMod 1 test_import_attr)
          (AttrImport 2 hoge_module (
            (f 0)
            (g 1)
            (h 2)))
          FormEof)))) |}]
