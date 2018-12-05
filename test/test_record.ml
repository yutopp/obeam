open Test_util

let%expect_test "test_record.beam" =
  print_ast "test_record.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile 1 test_record.erl 1)
          (AttrMod 1 test_record)
          (DeclRecord 3 (
            (3 a
              ()
              ())
            (4 b ((ExprLit (LitInteger 4 42))) ())
            (5 c () ((TyPredef 5 string ())))
            (6 d ((ExprLit (LitInteger 6 57))) ((TyPredef 6 integer ())))))
          FormEof)))) |}]
