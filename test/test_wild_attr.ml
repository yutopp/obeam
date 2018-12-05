open Test_util

let%expect_test "test_wild_attr.beam" =
  print_ast "test_wild_attr.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile 1 test_wild_attr.erl 1)
          (AttrMod 1 test_wild_attr)
          (AttrWild 3 compile (List ((Atom export_all))))
          (AttrWild 5 vsn (Atom 1.0.0))
          (AttrWild 7 on_load (
            Tuple 2 (
              (Atom    f)
              (Integer 0))))
          (AttrWild 9  behaviour (Atom   gen_server))
          (AttrWild 11 foo       (Binary bar))
          (DeclFun
           13
           f
           0
           ((
             ClsFun 13
             ()
             ()
             (ExprBody ((ExprLit (LitAtom 13 ok)))))))
          FormEof)))) |}]
