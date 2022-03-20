module Shape = {
  [@deriving show]
  type t =
    | Op
    | Pre(Precedence.t)
    | Post(Precedence.t)
    | Bin(Precedence.t);
};

module Sorts = {
  [@deriving show]
  type t = {
    out: Sort.t,
    in_: list(Sort.t),
  };
  let mk = (~in_=[], out) => {out, in_};
};

[@deriving show]
type t = {
  shape: Shape.t,
  sorts: Sorts.t,
};

let mk_op = sorts => {sorts, shape: Op};
let mk_pre = (p, sorts) => {sorts, shape: Pre(p)};
let mk_post = (p, sorts) => {sorts, shape: Post(p)};
let mk_bin = (p, sorts) => {sorts, shape: Bin(p)};

let nibs: (~index: int=?, t) => Nibs.t =
  (~index as _=?, _) => failwith("todo Mold.nibs");
