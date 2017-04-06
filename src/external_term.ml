(*
 * Copyright yutopp 2017 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

let uncompress_form size buf =
  (* for input *)
  let pos = ref 0 in
  let fill in_buf =
    let buf_size = Bytes.length in_buf in
    let rest_size = (Bitstring.bitstring_length buf / 8) - (!pos*8) in
    let size = min buf_size rest_size in
    let subbitstr = Bitstring.subbitstring buf (!pos*8) (size*8) in
    Bytes.blit_string (subbitstr |> Bitstring.string_of_bitstring) 0 in_buf 0 size;
    pos := !pos + size;
    size
  in
  (* for output *)
  let out_mem = Buffer.create size in
  let export out_buf len =
    Buffer.add_bytes out_mem out_buf
  in
  (* uncompress *)
  Zlib.uncompress fill export;
  (* to bitstring *)
  Buffer.sub out_mem 0 size
  |> Bitstring.bitstring_of_string


type t =
  | SmallInteger of int
  | Integer of int32
  | Atom of string
  | SmallTuple of int * t list
  | Nil
  | String of string
  | List of t list * t
  | NewFloat of float
[@@deriving show]


let rec parse_etf (_, buf) =
  let open Combinator in
  match%bitstring buf with
  (* compressed *)
  | {| 80   : 1*8
     ; size : 4*8 : bigendian
     ; buf  : -1 : bitstring
     |} ->
     let data = uncompress_form ((Int32.to_int size)+1) buf in
     parse_etf ([], data)

  (* SMALL_INTEGER_EXT *)
  | {| 97    : 1*8
     ; value : 1*8
     ; rest  : -1 : bitstring
     |} ->
     Ok (SmallInteger value, rest)

  (* INTEGER_EXT *)
  | {| 98    : 1*8
     ; value : 4*8 : bigendian
     ; rest  : -1 : bitstring
     |} ->
     Ok (Integer value, rest)

  (* ATOM_EXT *)
  | {| 100  : 1*8
     ; len  : 2*8
     ; name : len * 8 : string
     ; rest : -1 : bitstring
     |} ->
     Ok (Atom name, rest)

  (* SMALL_TUPLE_EXT *)
  | {| 104      : 1*8
     ; arity    : 1*8
     ; elem_buf : -1 : bitstring
     |} ->
     list parse_etf arity elem_buf
     |> map (fun list -> SmallTuple (arity, list))

  (* NIL_EXT *)
  | {| 106   : 1*8
     ; rest  : -1 : bitstring
     |} ->
     Ok (Nil, rest)

  (* STRING_EXT *)
  | {| 107   : 1*8
     ; len   : 2*8
     ; chars : len*8 : string
     ; rest  : -1 : bitstring
     |} ->
     Ok (String chars, rest)

  (* LIST_EXT *)
  | {| 108      : 1*8
     ; len      : 4*8
     ; list_buf : -1 : bitstring
     |} ->
     let parser =
       (* elements *)
       list parse_etf (Int32.to_int len)
       (* tail *)
       >> act parse_etf (fun n p -> (p, n))
     in
     parser list_buf |> map (fun (list, tail) -> List (list, tail))

  (* NEW_FLOAT_EXT *)
  | {| 70    : 1*8
     ; value : 8*8
     ; rest  : -1 : bitstring
     |} ->
     Ok (NewFloat (Int64.float_of_bits value), rest)

  (* unknown *)
  | {| head : 1*8; _ |} ->
     Error (Printf.sprintf "error (%d)" head)

let parse buf =
  match%bitstring buf with
  | {| 131  : 1*8
     ; rest : -1 : bitstring
     |} ->
     parse_etf ([], rest)
  | {| _ |} ->
     Error "unsupported version"
