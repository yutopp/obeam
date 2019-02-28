open Test_util

let%expect_test "test_bitstring.beam" =
  print_ast "test_bitstring.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_bitstring.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_bitstring))
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
                    ExprBitstr
                    (line 7)
                    (elements (
                      ((ExprLit (
                         lit (
                           LitInteger
                           (line    7)
                           (integer 0))))
                       ()
                       ())
                      ((ExprLit (
                         lit (
                           LitInteger
                           (line    8)
                           (integer 1))))
                       ((
                         ExprLit (
                           lit (
                             LitInteger
                             (line    8)
                             (integer 1)))))
                       ())
                      ((ExprLit (
                         lit (
                           LitInteger
                           (line    9)
                           (integer 2))))
                       ()
                       ((
                         (TypeSpec (atom little)  (value ()))
                         (TypeSpec (atom signed)  (value ()))
                         (TypeSpec (atom integer) (value ())))))
                      ((ExprLit (
                         lit (
                           LitInteger
                           (line    10)
                           (integer 3))))
                       ((
                         ExprLit (
                           lit (
                             LitInteger
                             (line    10)
                             (integer 3)))))
                       ((
                         (TypeSpec (atom little) (value ()))
                         (TypeSpec (atom signed) (value ()))
                         (TypeSpec (atom unit) (value (8)))
                         (TypeSpec (atom integer) (value ())))))
                      ((ExprBinOp
                         (line 11)
                         (op   +)
                         (lhs (
                           ExprLit (
                             lit (
                               LitInteger
                               (line    11)
                               (integer 2)))))
                         (rhs (
                           ExprLit (
                             lit (
                               LitInteger
                               (line    11)
                               (integer 2))))))
                       ((
                         ExprBinOp
                         (line 11)
                         (op   +)
                         (lhs (
                           ExprLit (
                             lit (
                               LitInteger
                               (line    11)
                               (integer 2)))))
                         (rhs (
                           ExprLit (
                             lit (
                               LitInteger
                               (line    11)
                               (integer 2)))))))
                       ()))))))))))))
          FormEof)))) |}]
