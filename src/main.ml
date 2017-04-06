(*
 * Copyright yutopp 2017 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

let () =
  let beam_buf = Bitstring.bitstring_of_file "test.beam" in
  match Chunk_parser.parse_layout beam_buf with
  | Ok (layout, _) ->
     let {
       Chunk_parser.cl_abst = opt_abst;
     } = layout in
     let abst =
       match opt_abst with
       | Some abst -> abst
       | None -> failwith "abst chunk is not found"
     in
     let _ =
       match External_term.parse abst.Chunk_parser.abst_buf with
       | Ok (expr, _) ->
          Printf.printf "%s\n" (expr |> External_term.show);
          Printf.printf "%s\n" (expr |> Ast.of_etf |> Ast.show);
       | Error msg ->
          Printf.printf "Failed: %s" msg
     in
     ()
  | Error msg ->
     Printf.printf "Failed : %s\n" msg;
