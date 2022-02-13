open Sexplib.Std;

module Shape = {
  [@deriving sexp]
  type t =
    | Op
    | Pre
    | Post
    | Bin;
};

module Sorts = {
  [@deriving sexp]
  type t = {
    out: Sort.t,
    in_: list(Sort.t),
  };
  let mk = (~in_=[], out) => {out, in_};
};

[@deriving sexp]
type t = {
  precedence: Precedence.t,
  shape: Shape.t,
  sorts: Sorts.t,
};

let mk_op = sorts => {sorts, shape: Op, precedence: Precedence.max_p};
let mk_pre = (precedence, sorts) => {sorts, precedence, shape: Pre};
let mk_post = (precedence, sorts) => {sorts, precedence, shape: Post};
let mk_bin = (precedence, sorts) => {sorts, precedence, shape: Bin};
