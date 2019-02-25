open Test_util

let%expect_test "test_try_expr.beam" =
  print_ast "test_try_expr.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_try_expr.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_try_expr))
          (AttrExport
            (line 3)
            (function_arity_list (
              (try_catch          0)
              (try_of_catch       0)
              (try_after          0)
              (try_of_after       0)
              (try_catch_after    0)
              (try_of_catch_after 0))))
          (DeclFun
            (line          17)
            (function_name try_catch)
            (arity         0)
            (clauses ((
              ClsFun
              (line 17)
              (patterns       ())
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprTry
                    (line 18)
                    (exprs (
                      (ExprMatch
                        (line 19)
                        (pattern (
                          PatVar
                          (line 19)
                          (id   R)))
                        (body (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 19)
                              (atom reason))))))
                      (ExprLocalCall
                        (line 20)
                        (function_expr (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 20)
                              (atom error)))))
                        (args ((
                          ExprVar
                          (line 20)
                          (id   R)))))))
                    (case_clauses ())
                    (catch_clauses (
                      (ClsCatch
                        (line            22)
                        (line_cls        22)
                        (line_stacktrace 22)
                        (exception_class (AtomVarAtom (line 22) (atom throw)))
                        (pattern         (PatVar      (line 22) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ((
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 22)
                                  (op   =:=)
                                  (lhs (
                                    GuardTestVar
                                    (line 22)
                                    (id   Err)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitAtom
                                        (line 22)
                                        (atom error))))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 22)
                              (id   Err)))))))
                      (ClsCatch
                        (line            23)
                        (line_cls        23)
                        (line_stacktrace 23)
                        (exception_class (AtomVarAtom (line 23) (atom error)))
                        (pattern         (PatVar      (line 23) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ((
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 23)
                                  (op   =:=)
                                  (lhs (
                                    GuardTestVar
                                    (line 23)
                                    (id   Err)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitAtom
                                        (line 23)
                                        (atom error))))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 23)
                              (id   Err)))))))
                      (ClsCatch
                        (line            24)
                        (line_cls        24)
                        (line_stacktrace 24)
                        (exception_class (AtomVarVar (line 24) (id Class)))
                        (pattern         (PatVar     (line 24) (id Err)))
                        (stacktrace Stacktrace)
                        (guard_sequence ((
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 24)
                                  (op   =:=)
                                  (lhs (
                                    GuardTestVar
                                    (line 24)
                                    (id   Err)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitAtom
                                        (line 24)
                                        (atom error))))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprTuple
                              (line 24)
                              (elements (
                                (ExprVar (line 24) (id Class))
                                (ExprVar (line 24) (id Err))
                                (ExprVar (line 24) (id Stacktrace))))))))))
                      (ClsCatch
                        (line            25)
                        (line_cls        25)
                        (line_stacktrace 25)
                        (exception_class (AtomVarAtom (line 25) (atom throw)))
                        (pattern         (PatVar      (line 25) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 25)
                              (id   Err)))))))
                      (ClsCatch
                        (line            26)
                        (line_cls        26)
                        (line_stacktrace 26)
                        (exception_class (AtomVarAtom (line 26) (atom error)))
                        (pattern         (PatVar      (line 26) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 26)
                              (id   Err)))))))
                      (ClsCatch
                        (line            27)
                        (line_cls        27)
                        (line_stacktrace 27)
                        (exception_class (AtomVarVar (line 27) (id Class)))
                        (pattern         (PatVar     (line 27) (id Err)))
                        (stacktrace Stacktrace)
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprTuple
                              (line 27)
                              (elements (
                                (ExprVar (line 27) (id Class))
                                (ExprVar (line 27) (id Err))
                                (ExprVar (line 27) (id Stacktrace))))))))))))
                    (after ()))))))))))
          (DeclFun
            (line          31)
            (function_name try_of_catch)
            (arity         0)
            (clauses ((
              ClsFun
              (line 31)
              (patterns       ())
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprTry
                    (line 32)
                    (exprs (
                      (ExprMatch
                        (line 33)
                        (pattern (
                          PatVar
                          (line 33)
                          (id   R)))
                        (body (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 33)
                              (atom reason))))))
                      (ExprLocalCall
                        (line 34)
                        (function_expr (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 34)
                              (atom error)))))
                        (args ((
                          ExprVar
                          (line 34)
                          (id   R)))))))
                    (case_clauses (
                      (ClsCase
                        (line 36)
                        (pattern (
                          PatTuple
                          (line 36)
                          (pats (
                            (PatLit (
                              lit (
                                LitAtom
                                (line 36)
                                (atom ok))))
                            (PatVar
                              (line 36)
                              (id   A))))))
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 36)
                              (id   A)))))))
                      (ClsCase
                        (line 37)
                        (pattern (
                          PatLit (
                            lit (
                              LitAtom
                              (line 37)
                              (atom error)))))
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprLit (
                                lit (
                                  LitAtom
                                  (line 37)
                                  (atom error)))))))))))
                    (catch_clauses (
                      (ClsCatch
                        (line            39)
                        (line_cls        39)
                        (line_stacktrace 39)
                        (exception_class (AtomVarAtom (line 39) (atom throw)))
                        (pattern         (PatVar      (line 39) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ((
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 39)
                                  (op   =:=)
                                  (lhs (
                                    GuardTestVar
                                    (line 39)
                                    (id   Err)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitAtom
                                        (line 39)
                                        (atom error))))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 39)
                              (id   Err)))))))
                      (ClsCatch
                        (line            40)
                        (line_cls        40)
                        (line_stacktrace 40)
                        (exception_class (AtomVarAtom (line 40) (atom error)))
                        (pattern         (PatVar      (line 40) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ((
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 40)
                                  (op   =:=)
                                  (lhs (
                                    GuardTestVar
                                    (line 40)
                                    (id   Err)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitAtom
                                        (line 40)
                                        (atom error))))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 40)
                              (id   Err)))))))
                      (ClsCatch
                        (line            41)
                        (line_cls        41)
                        (line_stacktrace 41)
                        (exception_class (AtomVarVar (line 41) (id Class)))
                        (pattern         (PatVar     (line 41) (id Err)))
                        (stacktrace Stacktrace)
                        (guard_sequence ((
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 41)
                                  (op   =:=)
                                  (lhs (
                                    GuardTestVar
                                    (line 41)
                                    (id   Err)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitAtom
                                        (line 41)
                                        (atom error))))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprTuple
                              (line 41)
                              (elements (
                                (ExprVar (line 41) (id Class))
                                (ExprVar (line 41) (id Err))
                                (ExprVar (line 41) (id Stacktrace))))))))))
                      (ClsCatch
                        (line            42)
                        (line_cls        42)
                        (line_stacktrace 42)
                        (exception_class (AtomVarAtom (line 42) (atom throw)))
                        (pattern         (PatVar      (line 42) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 42)
                              (id   Err)))))))
                      (ClsCatch
                        (line            43)
                        (line_cls        43)
                        (line_stacktrace 43)
                        (exception_class (AtomVarAtom (line 43) (atom error)))
                        (pattern         (PatVar      (line 43) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 43)
                              (id   Err)))))))
                      (ClsCatch
                        (line            44)
                        (line_cls        44)
                        (line_stacktrace 44)
                        (exception_class (AtomVarVar (line 44) (id Class)))
                        (pattern         (PatVar     (line 44) (id Err)))
                        (stacktrace Stacktrace)
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprTuple
                              (line 44)
                              (elements (
                                (ExprVar (line 44) (id Class))
                                (ExprVar (line 44) (id Err))
                                (ExprVar (line 44) (id Stacktrace))))))))))))
                    (after ()))))))))))
          (DeclFun
            (line          48)
            (function_name try_after)
            (arity         0)
            (clauses ((
              ClsFun
              (line 48)
              (patterns       ())
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprTry
                    (line 49)
                    (exprs (
                      (ExprMatch
                        (line 50)
                        (pattern (
                          PatVar
                          (line 50)
                          (id   R)))
                        (body (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 50)
                              (atom reason))))))
                      (ExprLocalCall
                        (line 51)
                        (function_expr (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 51)
                              (atom error)))))
                        (args ((
                          ExprVar
                          (line 51)
                          (id   R)))))))
                    (case_clauses  ())
                    (catch_clauses ())
                    (after (
                      (ExprMatch
                        (line 53)
                        (pattern (
                          PatVar
                          (line 53)
                          (id   Ok)))
                        (body (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 53)
                              (atom ok))))))
                      (ExprVar
                        (line 54)
                        (id   Ok)))))))))))))
          (DeclFun
            (line          58)
            (function_name try_of_after)
            (arity         0)
            (clauses ((
              ClsFun
              (line 58)
              (patterns       ())
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprTry
                    (line 59)
                    (exprs (
                      (ExprMatch
                        (line 60)
                        (pattern (
                          PatVar
                          (line 60)
                          (id   R)))
                        (body (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 60)
                              (atom reason))))))
                      (ExprLocalCall
                        (line 61)
                        (function_expr (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 61)
                              (atom error)))))
                        (args ((
                          ExprVar
                          (line 61)
                          (id   R)))))))
                    (case_clauses (
                      (ClsCase
                        (line 63)
                        (pattern (
                          PatTuple
                          (line 63)
                          (pats (
                            (PatLit (
                              lit (
                                LitAtom
                                (line 63)
                                (atom ok))))
                            (PatVar
                              (line 63)
                              (id   A))))))
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 63)
                              (id   A)))))))
                      (ClsCase
                        (line 64)
                        (pattern (
                          PatLit (
                            lit (
                              LitAtom
                              (line 64)
                              (atom error)))))
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprLit (
                                lit (
                                  LitAtom
                                  (line 64)
                                  (atom error)))))))))))
                    (catch_clauses ())
                    (after (
                      (ExprMatch
                        (line 66)
                        (pattern (
                          PatVar
                          (line 66)
                          (id   Ok)))
                        (body (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 66)
                              (atom ok))))))
                      (ExprVar
                        (line 67)
                        (id   Ok)))))))))))))
          (DeclFun
            (line          71)
            (function_name try_catch_after)
            (arity         0)
            (clauses ((
              ClsFun
              (line 71)
              (patterns       ())
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprTry
                    (line 72)
                    (exprs (
                      (ExprMatch
                        (line 73)
                        (pattern (
                          PatVar
                          (line 73)
                          (id   R)))
                        (body (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 73)
                              (atom reason))))))
                      (ExprLocalCall
                        (line 74)
                        (function_expr (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 74)
                              (atom error)))))
                        (args ((
                          ExprVar
                          (line 74)
                          (id   R)))))))
                    (case_clauses ())
                    (catch_clauses (
                      (ClsCatch
                        (line            76)
                        (line_cls        76)
                        (line_stacktrace 76)
                        (exception_class (AtomVarAtom (line 76) (atom throw)))
                        (pattern         (PatVar      (line 76) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ((
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 76)
                                  (op   =:=)
                                  (lhs (
                                    GuardTestVar
                                    (line 76)
                                    (id   Err)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitAtom
                                        (line 76)
                                        (atom error))))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 76)
                              (id   Err)))))))
                      (ClsCatch
                        (line            77)
                        (line_cls        77)
                        (line_stacktrace 77)
                        (exception_class (AtomVarAtom (line 77) (atom error)))
                        (pattern         (PatVar      (line 77) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ((
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 77)
                                  (op   =:=)
                                  (lhs (
                                    GuardTestVar
                                    (line 77)
                                    (id   Err)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitAtom
                                        (line 77)
                                        (atom error))))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 77)
                              (id   Err)))))))
                      (ClsCatch
                        (line            78)
                        (line_cls        78)
                        (line_stacktrace 78)
                        (exception_class (AtomVarVar (line 78) (id Class)))
                        (pattern         (PatVar     (line 78) (id Err)))
                        (stacktrace Stacktrace)
                        (guard_sequence ((
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 78)
                                  (op   =:=)
                                  (lhs (
                                    GuardTestVar
                                    (line 78)
                                    (id   Err)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitAtom
                                        (line 78)
                                        (atom error))))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprTuple
                              (line 78)
                              (elements (
                                (ExprVar (line 78) (id Class))
                                (ExprVar (line 78) (id Err))
                                (ExprVar (line 78) (id Stacktrace))))))))))
                      (ClsCatch
                        (line            79)
                        (line_cls        79)
                        (line_stacktrace 79)
                        (exception_class (AtomVarAtom (line 79) (atom throw)))
                        (pattern         (PatVar      (line 79) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 79)
                              (id   Err)))))))
                      (ClsCatch
                        (line            80)
                        (line_cls        80)
                        (line_stacktrace 80)
                        (exception_class (AtomVarAtom (line 80) (atom error)))
                        (pattern         (PatVar      (line 80) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 80)
                              (id   Err)))))))
                      (ClsCatch
                        (line            81)
                        (line_cls        81)
                        (line_stacktrace 81)
                        (exception_class (AtomVarVar (line 81) (id Class)))
                        (pattern         (PatVar     (line 81) (id Err)))
                        (stacktrace Stacktrace)
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprTuple
                              (line 81)
                              (elements (
                                (ExprVar (line 81) (id Class))
                                (ExprVar (line 81) (id Err))
                                (ExprVar (line 81) (id Stacktrace))))))))))))
                    (after (
                      (ExprMatch
                        (line 83)
                        (pattern (
                          PatVar
                          (line 83)
                          (id   Ok)))
                        (body (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 83)
                              (atom ok))))))
                      (ExprVar
                        (line 84)
                        (id   Ok)))))))))))))
          (DeclFun
            (line          88)
            (function_name try_of_catch_after)
            (arity         0)
            (clauses ((
              ClsFun
              (line 88)
              (patterns       ())
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprTry
                    (line 89)
                    (exprs (
                      (ExprMatch
                        (line 90)
                        (pattern (
                          PatVar
                          (line 90)
                          (id   R)))
                        (body (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 90)
                              (atom reason))))))
                      (ExprLocalCall
                        (line 91)
                        (function_expr (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 91)
                              (atom error)))))
                        (args ((
                          ExprVar
                          (line 91)
                          (id   R)))))))
                    (case_clauses (
                      (ClsCase
                        (line 93)
                        (pattern (
                          PatTuple
                          (line 93)
                          (pats (
                            (PatLit (
                              lit (
                                LitAtom
                                (line 93)
                                (atom ok))))
                            (PatVar
                              (line 93)
                              (id   A))))))
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 93)
                              (id   A)))))))
                      (ClsCase
                        (line 94)
                        (pattern (
                          PatLit (
                            lit (
                              LitAtom
                              (line 94)
                              (atom error)))))
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprLit (
                                lit (
                                  LitAtom
                                  (line 94)
                                  (atom error)))))))))))
                    (catch_clauses (
                      (ClsCatch
                        (line            96)
                        (line_cls        96)
                        (line_stacktrace 96)
                        (exception_class (AtomVarAtom (line 96) (atom throw)))
                        (pattern         (PatVar      (line 96) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ((
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 96)
                                  (op   =:=)
                                  (lhs (
                                    GuardTestVar
                                    (line 96)
                                    (id   Err)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitAtom
                                        (line 96)
                                        (atom error))))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 96)
                              (id   Err)))))))
                      (ClsCatch
                        (line            97)
                        (line_cls        97)
                        (line_stacktrace 97)
                        (exception_class (AtomVarAtom (line 97) (atom error)))
                        (pattern         (PatVar      (line 97) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ((
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 97)
                                  (op   =:=)
                                  (lhs (
                                    GuardTestVar
                                    (line 97)
                                    (id   Err)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitAtom
                                        (line 97)
                                        (atom error))))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 97)
                              (id   Err)))))))
                      (ClsCatch
                        (line            98)
                        (line_cls        98)
                        (line_stacktrace 98)
                        (exception_class (AtomVarVar (line 98) (id Class)))
                        (pattern         (PatVar     (line 98) (id Err)))
                        (stacktrace Stacktrace)
                        (guard_sequence ((
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 98)
                                  (op   =:=)
                                  (lhs (
                                    GuardTestVar
                                    (line 98)
                                    (id   Err)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitAtom
                                        (line 98)
                                        (atom error))))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprTuple
                              (line 98)
                              (elements (
                                (ExprVar (line 98) (id Class))
                                (ExprVar (line 98) (id Err))
                                (ExprVar (line 98) (id Stacktrace))))))))))
                      (ClsCatch
                        (line            99)
                        (line_cls        99)
                        (line_stacktrace 99)
                        (exception_class (AtomVarAtom (line 99) (atom throw)))
                        (pattern         (PatVar      (line 99) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 99)
                              (id   Err)))))))
                      (ClsCatch
                        (line            100)
                        (line_cls        100)
                        (line_stacktrace 100)
                        (exception_class (AtomVarAtom (line 100) (atom error)))
                        (pattern         (PatVar      (line 100) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 100)
                              (id   Err)))))))
                      (ClsCatch
                        (line            101)
                        (line_cls        101)
                        (line_stacktrace 101)
                        (exception_class (AtomVarVar (line 101) (id Class)))
                        (pattern         (PatVar     (line 101) (id Err)))
                        (stacktrace Stacktrace)
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprTuple
                              (line 101)
                              (elements (
                                (ExprVar (line 101) (id Class))
                                (ExprVar (line 101) (id Err))
                                (ExprVar (line 101) (id Stacktrace))))))))))))
                    (after (
                      (ExprMatch
                        (line 103)
                        (pattern (
                          PatVar
                          (line 103)
                          (id   Ok)))
                        (body (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 103)
                              (atom ok))))))
                      (ExprVar
                        (line 104)
                        (id   Ok)))))))))))))
          FormEof)))) |}]
