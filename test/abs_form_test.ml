(*
 * Copyright yutopp 2017 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open OUnit2

(* cheker utils *)
let assert_equals_abs_form_by_erl_file expect_ast filename test_ctx =
  let open Obeam in
  let beam_filename = Util.generate_beam_from_erl filename in
  let beam_buf = Bitstring.bitstring_of_file beam_filename in
  match Chunk.parse_layout beam_buf with
  | Ok (layout, _) ->
     let {
       Chunk.cl_abst = opt_abst;
       Chunk.cl_dbgi = opt_dbgi;
     } = layout in
     let parse_result =
       match opt_abst with
       | Some abst ->
          External_term_format.parse abst.Chunk.abst_buf
       | None ->
          begin
            match opt_dbgi with
            | Some dbgi ->
               External_term_format.parse dbgi.Chunk.dbgi_buf
            | None ->
               failwith "abst and dbgi chunk is not found"
          end
     in
     let _ =
       match parse_result with
       | Ok (etf, _) ->
          (*let () = etf |> External_term.show |> Printf.printf "%s\n" in*)
          let actual_ast = etf |> Abstract_format.of_etf in
          assert_equal ?printer:(Some Abstract_format.show)
                       expect_ast actual_ast
       | Error (msg, _rest) ->
          failwith (Printf.sprintf "Failed: %s" msg)
     in
     ()
  | Error (msg, _rest) ->
     failwith (Printf.sprintf "Failed : %s\n" msg)

let assert_equals_abs_form_f f test_ctx =
  let (filename, expect_ast) = f () in
  assert_equals_abs_form_by_erl_file expect_ast filename test_ctx

(* test cases *)
let template_01 () =
  let module Ast = Obeam.Abstract_format in
  ("test01.erl",
   Ast.AbstractCode
     (Ast.ModDecl
        [(Ast.AttrFile (1, "test01.erl", 1)); (Ast.AttrMod (1, "test01"));
         (Ast.AttrExport (3, [("g", 0); ("h", 1)]));
         (Ast.DeclFun (5, "f", 0,
                       [(Ast.ClsFun (5, [], None,
                                     (Ast.ExprBody
                                        [(Ast.ExprLit (Ast.LitInteger (6, 0)))])))
                       ]
                      ));
         (Ast.DeclFun (8, "g", 0,
                       [(Ast.ClsFun (8, [], None,
                                     (Ast.ExprBody
                                        [(Ast.ExprLit (Ast.LitInteger (9, 10)))])))
                       ]
                      ));
         (Ast.SpecFun (11, None, "h", 1,
                       [(Ast.TyFun
                           (11,
                            (Ast.TyProduct (11, [(Ast.TyPredef (11, "integer", []))])),
                            (Ast.TyPredef (11, "string", []))))
                       ]
                      ));
         (Ast.DeclFun (12, "h", 1,
                       [(Ast.ClsFun (12, [(Ast.PatUniversal 12)], None,
                                     (Ast.ExprBody
                                        [(Ast.ExprLit
                                            (Ast.LitString (13, "abcdefg")))
                                        ])
                                    ))
                       ]
                      ));
         Ast.FormEof])
  )

let template_02 () =
  let module Ast = Obeam.Abstract_format in
  ("test02.erl",
   Ast.AbstractCode
     (Ast.ModDecl
        [(Ast.AttrFile (1, "test02.erl", 1)); (Ast.AttrMod (1, "test02"));
         (Ast.SpecFun (3, None, "i", 1,
                       [(Ast.TyContFun
                           (3,
                            (Ast.TyPredef (3, "fun",
                                           [(Ast.TyProduct (3, [(Ast.TyVar (3, "A"))]));
                                            (Ast.TyPredef (3, "integer", []))]
                                          )),
                            (Ast.TyCont
                               [(Ast.TyContRel
                                   (3, (Ast.TyContIsSubType 3),
                                    (Ast.TyVar (3, "A")),
                                    (Ast.TyPredef (3, "integer", []))
                                   ))
                               ])
                           ))
                       ]
                      ));
         (Ast.DeclFun (4, "i", 1,
                       [(Ast.ClsFun (4, [(Ast.PatVar (4, "N"))], None,
                                     (Ast.ExprBody [(Ast.ExprVar (4, "N"))])))
                       ]
                      ));
         (Ast.SpecFun (6, None, "j", 1,
                       [(Ast.TyContFun
                           (6,
                            (Ast.TyPredef (6, "fun",
                                           [(Ast.TyProduct (6, [(Ast.TyVar (6, "A"))]));
                                            (Ast.TyVar (6, "B"))]
                                          )),
                            (Ast.TyCont
                               [(Ast.TyContRel
                                   (6, (Ast.TyContIsSubType 6),
                                    (Ast.TyVar (6, "A")),
                                    (Ast.TyPredef (6, "integer", []))
                                   ));
                                (Ast.TyContRel
                                   (7, (Ast.TyContIsSubType 7),
                                    (Ast.TyVar (7, "B")),
                                    (Ast.TyPredef (7, "integer", []))))
                               ])
                           ))
                       ]
                      ));
         (Ast.DeclFun (8, "j", 1,
                       [(Ast.ClsFun
                           (8, [(Ast.PatVar (8, "N"))], None,
                            (Ast.ExprBody
                               [(Ast.ExprBinOp (9, "*", (Ast.ExprVar (9, "N")),
                                                (Ast.ExprVar (9, "N"))))
                               ])
                           ))
                       ]
                      ));
         Ast.FormEof])
  )

let template_03 () =
  let module Ast = Obeam.Abstract_format in
  ("test03.erl",
   Ast.AbstractCode
     (Ast.ModDecl
        [(Ast.AttrFile (1, "test03.erl", 1)); (Ast.AttrMod (1, "test03"));
         (Ast.DeclFun (3, "f", 1,
                       [(Ast.ClsFun
                           (3, [(Ast.PatVar (3, "N"))],
                            (Some (Ast.GuardSeq
                                     [(Ast.Guard
                                         [(Ast.GuardTestCall
                                             (3,
                                              (Ast.LitAtom (3, "is_integer")),
                                              [(Ast.GuardTestVar (3, "N"))]))
                                         ])
                                     ])),
                            (Ast.ExprBody [(Ast.ExprLit (Ast.LitInteger (4, 0)))])));
                        (Ast.ClsFun
                           (5, [(Ast.PatUniversal 5)],
                            None,
                            (Ast.ExprBody [(Ast.ExprLit (Ast.LitInteger (6, 2)))])))
                       ]
                      ));
         (Ast.DeclFun (8, "g", 1,
                       [(Ast.ClsFun
                           (8, [(Ast.PatVar (8, "R"))],
                            None,
                            (Ast.ExprBody
                               [(Ast.ExprCase
                                   (9,
                                    (Ast.ExprVar (9, "R")),
                                    [(Ast.ClsCase
                                        (10,
                                         (Ast.PatLit
                                            (Ast.LitAtom (10, "ok"))),
                                         None,
                                         (Ast.ExprBody
                                            [(Ast.ExprLit
                                                (Ast.LitInteger (10, 1)))
                                         ])
                                     ));
                                     (Ast.ClsCase
                                        (11,
                                         (Ast.PatLit
                                            (Ast.LitAtom (11, "error"))),
                                         None,
                                         (Ast.ExprBody
                                            [(Ast.ExprLit
                                                (Ast.LitInteger (11, 2)))
                                         ])
                                     ))
                                    ]
                                ))
                            ])
                        ))
                       ]
         ));
         Ast.FormEof])
  )

let template_04 () =
  let module Ast = Obeam.Abstract_format in
  ("test04.erl",
   Ast.AbstractCode
   (Ast.ModDecl
      [(Ast.AttrFile (1, "test04.erl", 1));
        (Ast.AttrMod (1, "test04"));
        (Ast.AttrExport (3, [("f", 0); ("g", 1); ("h", 1)]));
        (Ast.DeclFun (5, "f", 0,
           [(Ast.ClsFun (5, [], None,
               (Ast.ExprBody
                  [(Ast.ExprMapUpdate (5,
                      (Ast.ExprMapCreation (5,
                         [(Ast.ExprAssoc (5,
                             (Ast.ExprLit
                                (Ast.LitAtom (5, "a"))),
                             (Ast.ExprLit
                                (Ast.LitInteger (5, 1)))
                             ));
                           (Ast.ExprAssoc (5,
                              (Ast.ExprLit
                                 (Ast.LitAtom (5, "b"))),
                              (Ast.ExprLit
                                 (Ast.LitInteger (5, 2)))
                              ))
                           ]
                         )),
                      [(Ast.ExprAssocExact (5,
                          (Ast.ExprLit
                             (Ast.LitAtom (5, "a"))),
                          (Ast.ExprLit
                             (Ast.LitInteger (5, 42)))
                          ));
                        (Ast.ExprAssoc (5,
                           (Ast.ExprLit
                              (Ast.LitAtom (5, "c"))),
                           (Ast.ExprLit
                              (Ast.LitInteger (5, 3)))
                           ))
                        ]
                      ))
                    ])
               ))
             ]
           ));
        (Ast.DeclFun (7, "g", 1,
           [(Ast.ClsFun (7,
               [(Ast.PatMap (7,
                   [(Ast.PatAssocExact (7,
                       (Ast.PatLit
                          (Ast.LitAtom (7, "a"))),
                       (Ast.PatVar (7, "N"))))
                     ]
                   ))
                 ],
               None,
               (Ast.ExprBody [(Ast.ExprVar (7, "N"))])
               ))
             ]
           ));
        (Ast.DeclFun (9, "h", 1,
           [(Ast.ClsFun (9, [(Ast.PatVar (9, "M"))],
               (Some (Ast.GuardSeq
                        [(Ast.Guard
                            [(Ast.GuardTestBinOp (9, "andalso",
                                (Ast.GuardTestBinOp (9, "=:=",
                                   (Ast.GuardTestVar (9, "M")),
                                   (Ast.GuardTestMapCreation (9,
                                      [(Ast.GuardTestAssoc (9,
                                          (Ast.GuardTestLit
                                             (Ast.LitAtom (9, "a"
                                                ))),
                                          (Ast.GuardTestLit
                                             (Ast.LitInteger (9,
                                                42)))
                                          ))
                                        ]
                                      ))
                                   )),
                                (Ast.GuardTestBinOp (9, "=:=",
                                   (Ast.GuardTestMapUpdate (9,
                                      (Ast.GuardTestVar (9, "M")),
                                      [(Ast.GuardTestAssocExact (
                                          9,
                                          (Ast.GuardTestLit
                                             (Ast.LitAtom (9, "a"
                                                ))),
                                          (Ast.GuardTestLit
                                             (Ast.LitInteger (9,
                                                0)))
                                          ))
                                        ]
                                      )),
                                   (Ast.GuardTestMapCreation (9,
                                      [(Ast.GuardTestAssoc (9,
                                          (Ast.GuardTestLit
                                             (Ast.LitAtom (9, "a"
                                                ))),
                                          (Ast.GuardTestLit
                                             (Ast.LitInteger (9,
                                                0)))
                                          ))
                                        ]
                                      ))
                                   ))
                                ))
                              ])
                          ])),
               (Ast.ExprBody [(Ast.ExprVar (9, "M"))])
               ))
             ]
           ));
        Ast.FormEof])
  )

let template_05 () =
  let module Ast = Obeam.Abstract_format in
  ("test05.erl",
   Ast.AbstractCode
     (Ast.ModDecl
        [(Ast.AttrFile (1, "test05.erl", 1)); (Ast.AttrMod (1, "test05"));
         (Ast.DeclRecord (3, [(3, "a", None, None);
                              (4, "b", Some (Ast.ExprLit (Ast.LitInteger (4, 42))), None);
                              (5, "c", None, Some (Ast.TyPredef (5, "string", [])));
                              (6, "d", Some (Ast.ExprLit (Ast.LitInteger (6, 57))), Some (Ast.TyPredef (6, "integer", [])))]));
         Ast.FormEof])
  )

let rec suite =
  "parse_abs_form_in_beam_suite" >:::
    [
      "template_01" >:: assert_equals_abs_form_f template_01;
      "template_02" >:: assert_equals_abs_form_f template_02;
      "template_03" >:: assert_equals_abs_form_f template_03;
      "template_04" >:: assert_equals_abs_form_f template_04;
      "template_05" >:: assert_equals_abs_form_f template_05;
    ]

let () =
  run_test_tt_main suite
