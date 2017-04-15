(*
 * Copyright yutopp 2017 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

let generate_beam_from_erl in_filename =
  let out_filename =
    (in_filename |> Filename.remove_extension) ^ ".beam"
  in
  let code =
    Sys.command (Printf.sprintf "erlc +debug_info -o %s %s"
                                (Filename.quote out_filename)
                                (Filename.quote in_filename))
  in
  match code with
  | 0 -> out_filename
  | _ -> failwith (Printf.sprintf "Failed to execute erlc")
