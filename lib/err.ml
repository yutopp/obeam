(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type 'a t = {
    reason: 'a;
    loc: Source_code_position.t;
    previous: 'a t option;
  }
[@@deriving sexp_of]

let create ~loc reason =
  {
    reason = reason;
    loc = loc;
    previous = None;
  }

let wrap ~loc reason previous =
  let err = create ~loc reason in
  {err with previous = Some previous}
