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
            (line          7)
            (function_name try_catch)
            (arity         0)
            (clauses ((
              ClsFun
              (line 7)
              (patterns       ())
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprTry
                    (line 8)
                    (exprs (
                      (ExprMatch
                        (line 9)
                        (pattern (
                          PatVar
                          (line 9)
                          (id   R)))
                        (body (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 9)
                              (atom reason))))))
                      (ExprLocalCall
                        (line 10)
                        (function_expr (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 10)
                              (atom error)))))
                        (args ((
                          ExprVar
                          (line 10)
                          (id   R)))))))
                    (case_clauses ())
                    (catch_clauses (
                      (ClsCatch
                        (line            12)
                        (line_cls        12)
                        (line_stacktrace 12)
                        (exception_class (AtomVarAtom (line 12) (atom throw)))
                        (pattern         (PatVar      (line 12) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ((
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 12)
                                  (op   =:=)
                                  (lhs (
                                    GuardTestVar
                                    (line 12)
                                    (id   Err)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitAtom
                                        (line 12)
                                        (atom error))))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 12)
                              (id   Err)))))))
                      (ClsCatch
                        (line            13)
                        (line_cls        13)
                        (line_stacktrace 13)
                        (exception_class (AtomVarAtom (line 13) (atom error)))
                        (pattern         (PatVar      (line 13) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ((
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 13)
                                  (op   =:=)
                                  (lhs (
                                    GuardTestVar
                                    (line 13)
                                    (id   Err)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitAtom
                                        (line 13)
                                        (atom error))))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 13)
                              (id   Err)))))))
                      (ClsCatch
                        (line            14)
                        (line_cls        14)
                        (line_stacktrace 14)
                        (exception_class (AtomVarVar (line 14) (id Class)))
                        (pattern         (PatVar     (line 14) (id Err)))
                        (stacktrace Stacktrace)
                        (guard_sequence ((
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 14)
                                  (op   =:=)
                                  (lhs (
                                    GuardTestVar
                                    (line 14)
                                    (id   Err)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitAtom
                                        (line 14)
                                        (atom error))))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprTuple
                              (line 14)
                              (elements (
                                (ExprVar (line 14) (id Class))
                                (ExprVar (line 14) (id Err))
                                (ExprVar (line 14) (id Stacktrace))))))))))
                      (ClsCatch
                        (line            15)
                        (line_cls        15)
                        (line_stacktrace 15)
                        (exception_class (AtomVarAtom (line 15) (atom throw)))
                        (pattern         (PatVar      (line 15) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 15)
                              (id   Err)))))))
                      (ClsCatch
                        (line            16)
                        (line_cls        16)
                        (line_stacktrace 16)
                        (exception_class (AtomVarAtom (line 16) (atom error)))
                        (pattern         (PatVar      (line 16) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 16)
                              (id   Err)))))))
                      (ClsCatch
                        (line            17)
                        (line_cls        17)
                        (line_stacktrace 17)
                        (exception_class (AtomVarVar (line 17) (id Class)))
                        (pattern         (PatVar     (line 17) (id Err)))
                        (stacktrace Stacktrace)
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprTuple
                              (line 17)
                              (elements (
                                (ExprVar (line 17) (id Class))
                                (ExprVar (line 17) (id Err))
                                (ExprVar (line 17) (id Stacktrace))))))))))))
                    (after ()))))))))))
          (DeclFun
            (line          21)
            (function_name try_of_catch)
            (arity         0)
            (clauses ((
              ClsFun
              (line 21)
              (patterns       ())
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprTry
                    (line 22)
                    (exprs (
                      (ExprMatch
                        (line 23)
                        (pattern (
                          PatVar
                          (line 23)
                          (id   R)))
                        (body (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 23)
                              (atom reason))))))
                      (ExprLocalCall
                        (line 24)
                        (function_expr (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 24)
                              (atom error)))))
                        (args ((
                          ExprVar
                          (line 24)
                          (id   R)))))))
                    (case_clauses (
                      (ClsCase
                        (line 26)
                        (pattern (
                          PatTuple
                          (line 26)
                          (pats (
                            (PatLit (
                              lit (
                                LitAtom
                                (line 26)
                                (atom ok))))
                            (PatVar
                              (line 26)
                              (id   A))))))
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 26)
                              (id   A)))))))
                      (ClsCase
                        (line 27)
                        (pattern (
                          PatLit (
                            lit (
                              LitAtom
                              (line 27)
                              (atom error)))))
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprLit (
                                lit (
                                  LitAtom
                                  (line 27)
                                  (atom error)))))))))))
                    (catch_clauses (
                      (ClsCatch
                        (line            29)
                        (line_cls        29)
                        (line_stacktrace 29)
                        (exception_class (AtomVarAtom (line 29) (atom throw)))
                        (pattern         (PatVar      (line 29) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ((
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 29)
                                  (op   =:=)
                                  (lhs (
                                    GuardTestVar
                                    (line 29)
                                    (id   Err)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitAtom
                                        (line 29)
                                        (atom error))))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 29)
                              (id   Err)))))))
                      (ClsCatch
                        (line            30)
                        (line_cls        30)
                        (line_stacktrace 30)
                        (exception_class (AtomVarVar (line 30) (id Class)))
                        (pattern         (PatVar     (line 30) (id Err)))
                        (stacktrace _)
                        (guard_sequence ((
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 30)
                                  (op   =:=)
                                  (lhs (
                                    GuardTestVar
                                    (line 30)
                                    (id   Err)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitAtom
                                        (line 30)
                                        (atom error))))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 30)
                              (id   Err)))))))
                      (ClsCatch
                        (line            31)
                        (line_cls        31)
                        (line_stacktrace 31)
                        (exception_class (AtomVarVar (line 31) (id Class)))
                        (pattern         (PatVar     (line 31) (id Err)))
                        (stacktrace Stacktrace)
                        (guard_sequence ((
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 31)
                                  (op   =:=)
                                  (lhs (
                                    GuardTestVar
                                    (line 31)
                                    (id   Err)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitAtom
                                        (line 31)
                                        (atom error))))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprTuple
                              (line 31)
                              (elements (
                                (ExprVar (line 31) (id Class))
                                (ExprVar (line 31) (id Err))
                                (ExprVar (line 31) (id Stacktrace))))))))))
                      (ClsCatch
                        (line            32)
                        (line_cls        32)
                        (line_stacktrace 32)
                        (exception_class (AtomVarAtom (line 32) (atom throw)))
                        (pattern         (PatVar      (line 32) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 32)
                              (id   Err)))))))
                      (ClsCatch
                        (line            33)
                        (line_cls        33)
                        (line_stacktrace 33)
                        (exception_class (AtomVarAtom (line 33) (atom error)))
                        (pattern         (PatVar      (line 33) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 33)
                              (id   Err)))))))
                      (ClsCatch
                        (line            34)
                        (line_cls        34)
                        (line_stacktrace 34)
                        (exception_class (AtomVarVar (line 34) (id Class)))
                        (pattern         (PatVar     (line 34) (id Err)))
                        (stacktrace Stacktrace)
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprTuple
                              (line 34)
                              (elements (
                                (ExprVar (line 34) (id Class))
                                (ExprVar (line 34) (id Err))
                                (ExprVar (line 34) (id Stacktrace))))))))))))
                    (after ()))))))))))
          (DeclFun
            (line          38)
            (function_name try_after)
            (arity         0)
            (clauses ((
              ClsFun
              (line 38)
              (patterns       ())
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprTry
                    (line 39)
                    (exprs (
                      (ExprMatch
                        (line 40)
                        (pattern (
                          PatVar
                          (line 40)
                          (id   R)))
                        (body (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 40)
                              (atom reason))))))
                      (ExprLocalCall
                        (line 41)
                        (function_expr (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 41)
                              (atom error)))))
                        (args ((
                          ExprVar
                          (line 41)
                          (id   R)))))))
                    (case_clauses  ())
                    (catch_clauses ())
                    (after (
                      (ExprMatch
                        (line 43)
                        (pattern (
                          PatVar
                          (line 43)
                          (id   Ok)))
                        (body (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 43)
                              (atom ok))))))
                      (ExprVar
                        (line 44)
                        (id   Ok)))))))))))))
          (DeclFun
            (line          48)
            (function_name try_of_after)
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
                    (case_clauses (
                      (ClsCase
                        (line 53)
                        (pattern (
                          PatTuple
                          (line 53)
                          (pats (
                            (PatLit (
                              lit (
                                LitAtom
                                (line 53)
                                (atom ok))))
                            (PatVar
                              (line 53)
                              (id   A))))))
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 53)
                              (id   A)))))))
                      (ClsCase
                        (line 54)
                        (pattern (
                          PatLit (
                            lit (
                              LitAtom
                              (line 54)
                              (atom error)))))
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprLit (
                                lit (
                                  LitAtom
                                  (line 54)
                                  (atom error)))))))))))
                    (catch_clauses ())
                    (after (
                      (ExprMatch
                        (line 56)
                        (pattern (
                          PatVar
                          (line 56)
                          (id   Ok)))
                        (body (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 56)
                              (atom ok))))))
                      (ExprVar
                        (line 57)
                        (id   Ok)))))))))))))
          (DeclFun
            (line          61)
            (function_name try_catch_after)
            (arity         0)
            (clauses ((
              ClsFun
              (line 61)
              (patterns       ())
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprTry
                    (line 62)
                    (exprs (
                      (ExprMatch
                        (line 63)
                        (pattern (
                          PatVar
                          (line 63)
                          (id   R)))
                        (body (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 63)
                              (atom reason))))))
                      (ExprLocalCall
                        (line 64)
                        (function_expr (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 64)
                              (atom error)))))
                        (args ((
                          ExprVar
                          (line 64)
                          (id   R)))))))
                    (case_clauses ())
                    (catch_clauses (
                      (ClsCatch
                        (line            66)
                        (line_cls        66)
                        (line_stacktrace 66)
                        (exception_class (AtomVarAtom (line 66) (atom throw)))
                        (pattern         (PatVar      (line 66) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ((
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 66)
                                  (op   =:=)
                                  (lhs (
                                    GuardTestVar
                                    (line 66)
                                    (id   Err)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitAtom
                                        (line 66)
                                        (atom error))))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 66)
                              (id   Err)))))))
                      (ClsCatch
                        (line            67)
                        (line_cls        67)
                        (line_stacktrace 67)
                        (exception_class (AtomVarAtom (line 67) (atom error)))
                        (pattern         (PatVar      (line 67) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ((
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 67)
                                  (op   =:=)
                                  (lhs (
                                    GuardTestVar
                                    (line 67)
                                    (id   Err)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitAtom
                                        (line 67)
                                        (atom error))))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 67)
                              (id   Err)))))))
                      (ClsCatch
                        (line            68)
                        (line_cls        68)
                        (line_stacktrace 68)
                        (exception_class (AtomVarVar (line 68) (id Class)))
                        (pattern         (PatVar     (line 68) (id Err)))
                        (stacktrace Stacktrace)
                        (guard_sequence ((
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 68)
                                  (op   =:=)
                                  (lhs (
                                    GuardTestVar
                                    (line 68)
                                    (id   Err)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitAtom
                                        (line 68)
                                        (atom error))))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprTuple
                              (line 68)
                              (elements (
                                (ExprVar (line 68) (id Class))
                                (ExprVar (line 68) (id Err))
                                (ExprVar (line 68) (id Stacktrace))))))))))
                      (ClsCatch
                        (line            69)
                        (line_cls        69)
                        (line_stacktrace 69)
                        (exception_class (AtomVarAtom (line 69) (atom throw)))
                        (pattern         (PatVar      (line 69) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 69)
                              (id   Err)))))))
                      (ClsCatch
                        (line            70)
                        (line_cls        70)
                        (line_stacktrace 70)
                        (exception_class (AtomVarAtom (line 70) (atom error)))
                        (pattern         (PatVar      (line 70) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 70)
                              (id   Err)))))))
                      (ClsCatch
                        (line            71)
                        (line_cls        71)
                        (line_stacktrace 71)
                        (exception_class (AtomVarVar (line 71) (id Class)))
                        (pattern         (PatVar     (line 71) (id Err)))
                        (stacktrace Stacktrace)
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprTuple
                              (line 71)
                              (elements (
                                (ExprVar (line 71) (id Class))
                                (ExprVar (line 71) (id Err))
                                (ExprVar (line 71) (id Stacktrace))))))))))))
                    (after (
                      (ExprMatch
                        (line 73)
                        (pattern (
                          PatVar
                          (line 73)
                          (id   Ok)))
                        (body (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 73)
                              (atom ok))))))
                      (ExprVar
                        (line 74)
                        (id   Ok)))))))))))))
          (DeclFun
            (line          78)
            (function_name try_of_catch_after)
            (arity         0)
            (clauses ((
              ClsFun
              (line 78)
              (patterns       ())
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprTry
                    (line 79)
                    (exprs (
                      (ExprMatch
                        (line 80)
                        (pattern (
                          PatVar
                          (line 80)
                          (id   R)))
                        (body (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 80)
                              (atom reason))))))
                      (ExprLocalCall
                        (line 81)
                        (function_expr (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 81)
                              (atom error)))))
                        (args ((
                          ExprVar
                          (line 81)
                          (id   R)))))))
                    (case_clauses (
                      (ClsCase
                        (line 83)
                        (pattern (
                          PatTuple
                          (line 83)
                          (pats (
                            (PatLit (
                              lit (
                                LitAtom
                                (line 83)
                                (atom ok))))
                            (PatVar
                              (line 83)
                              (id   A))))))
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 83)
                              (id   A)))))))
                      (ClsCase
                        (line 84)
                        (pattern (
                          PatLit (
                            lit (
                              LitAtom
                              (line 84)
                              (atom error)))))
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprLit (
                                lit (
                                  LitAtom
                                  (line 84)
                                  (atom error)))))))))))
                    (catch_clauses (
                      (ClsCatch
                        (line            86)
                        (line_cls        86)
                        (line_stacktrace 86)
                        (exception_class (AtomVarAtom (line 86) (atom throw)))
                        (pattern         (PatVar      (line 86) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ((
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 86)
                                  (op   =:=)
                                  (lhs (
                                    GuardTestVar
                                    (line 86)
                                    (id   Err)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitAtom
                                        (line 86)
                                        (atom error))))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 86)
                              (id   Err)))))))
                      (ClsCatch
                        (line            87)
                        (line_cls        87)
                        (line_stacktrace 87)
                        (exception_class (AtomVarAtom (line 87) (atom error)))
                        (pattern         (PatVar      (line 87) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ((
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 87)
                                  (op   =:=)
                                  (lhs (
                                    GuardTestVar
                                    (line 87)
                                    (id   Err)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitAtom
                                        (line 87)
                                        (atom error))))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 87)
                              (id   Err)))))))
                      (ClsCatch
                        (line            88)
                        (line_cls        88)
                        (line_stacktrace 88)
                        (exception_class (AtomVarVar (line 88) (id Class)))
                        (pattern         (PatVar     (line 88) (id Err)))
                        (stacktrace Stacktrace)
                        (guard_sequence ((
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 88)
                                  (op   =:=)
                                  (lhs (
                                    GuardTestVar
                                    (line 88)
                                    (id   Err)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitAtom
                                        (line 88)
                                        (atom error))))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprTuple
                              (line 88)
                              (elements (
                                (ExprVar (line 88) (id Class))
                                (ExprVar (line 88) (id Err))
                                (ExprVar (line 88) (id Stacktrace))))))))))
                      (ClsCatch
                        (line            89)
                        (line_cls        89)
                        (line_stacktrace 89)
                        (exception_class (AtomVarAtom (line 89) (atom throw)))
                        (pattern         (PatVar      (line 89) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 89)
                              (id   Err)))))))
                      (ClsCatch
                        (line            90)
                        (line_cls        90)
                        (line_stacktrace 90)
                        (exception_class (AtomVarAtom (line 90) (atom error)))
                        (pattern         (PatVar      (line 90) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 90)
                              (id   Err)))))))
                      (ClsCatch
                        (line            91)
                        (line_cls        91)
                        (line_stacktrace 91)
                        (exception_class (AtomVarVar (line 91) (id Class)))
                        (pattern         (PatVar     (line 91) (id Err)))
                        (stacktrace Stacktrace)
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprTuple
                              (line 91)
                              (elements (
                                (ExprVar (line 91) (id Class))
                                (ExprVar (line 91) (id Err))
                                (ExprVar (line 91) (id Stacktrace))))))))))))
                    (after (
                      (ExprMatch
                        (line 93)
                        (pattern (
                          PatVar
                          (line 93)
                          (id   Ok)))
                        (body (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 93)
                              (atom ok))))))
                      (ExprVar
                        (line 94)
                        (id   Ok)))))))))))))
          FormEof)))) |}]
