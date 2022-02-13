module Id: {
  [@deriving sexp]
  type t = int;
  let compare: (t, t) => int;
};

module Label: {
  [@deriving sexp]
  type t = list(Token.t); // nonempty
};

module Map: {include Map.S with type key = Id.t;};

module Shape: {
  [@deriving sexp]
  type t =
    | Op
    | Pre
    | Post
    | Bin;
};

module Sorts: {
  [@deriving sexp]
  type t = {
    out: Sort.t,
    in_: list(Sort.t),
  };
  let mk: (~in_: list(Sort.t)=?, Sort.t) => t;
};

module Mold: {
  [@deriving sexp]
  type t = {
    precedence: Precedence.t,
    shape: Shape.t,
    sorts: Sorts.t,
  };
  let mk_op: Sorts.t => t;
  let mk_pre: (Precedence.t, Sorts.t) => t;
  let mk_post: (Precedence.t, Sorts.t) => t;
  let mk_bin: (Precedence.t, Sorts.t) => t;
};

module Shard: {
  [@deriving sexp]
  type index = int;
  [@deriving sexp]
  type labeled = {
    tile: (Id.t, Label.t),
    index: int,
    nibs: Nibs.t,
  };
  [@deriving sexp]
  type placeholder = {
    ids: list(Id.t),
    p: Precedence.t,
  };
  [@deriving sexp]
  type t =
    | Placeholder(placeholder)
    | Labeled(labeled);
};

[@deriving sexp]
type s = list(t)
and t =
  | Placeholder({substance: Util.Aba.t(Shard.t, s)})
  | Labeled(labeled)
// and placeholder =
and labeled = {
  id: Id.t,
  mold: Mold.t,
  substance: Util.Aba.t(Token.t, s),
};

// 1 + let x = 2 in x + 3
// --delete let tile-->
// 1 + () >< x >< 2 >< x + 3
// --construct lam-->
// 1 + \ x . 2 >< x + 3

let label: t => Label.t;

/**
 * to be raised if `assignable_molds(l)` contains
 * two molds for label `l` with the same nibs
 */
exception Ambiguous_molds;

let assignable_molds: (~l: Nib.t=?, Label.t) => list(Mold.t);
let default_mold: (Label.t, Sort.t, Sort.t) => Mold.t;

let nibs: (~index: int=?, Mold.t) => Nibs.t;

let reshape: t => list(t);

module Frame: {
  // example: y subject
  // 1 + let |y = x + 2 in y + 3

  // [let] _ [= x + 2 in]
  [@deriving sexp]
  type t = {
    id: Id.t,
    mold: Mold.t,
    substance: Util.Aba.Frame.B.t(Token.t, s),
  };

  [@deriving sexp]
  type step = int;
  let step: t => step;

  let label: t => Label.t;

  let sort: t => Sort.t;
};
