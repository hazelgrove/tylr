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
  (~index as _=?, {shape, sorts: {out: sort, _}}) => {
    let convex: Nib.t = {shape: Convex, sort};
    let concave: Precedence.t => Nib.t = p => {shape: Concave(p), sort};
    switch (shape) {
    | Op => (convex, convex)
    | Pre(p) => (convex, concave(p))
    | Post(p) => (concave(p), convex)
    | Bin(p) => (concave(p), concave(p))
    };
  };

module Map = {
  type mold = t;
  include Id.Map;
  type nonrec t = Id.Map.t(list(mold));
};

let of_grout: (Grout.t, Sort.t) => t =
  // TODO(andrew): dont do this?
  (g, sort) => {
    shape:
      switch (g) {
      | Convex => Op
      | Concave => Bin(Precedence.min)
      },
    sorts: {
      out: sort,
      in_: [],
    },
  };

let of_nibs: Nibs.t => t =
  // TODO(andrew): dont do this?
  ((l_nib, r_nib)) => {
    shape:
      switch (l_nib.shape, r_nib.shape) {
      | (Convex, Convex) => Op
      | (Concave(p), Concave(_)) => Bin(p)
      | (Convex, Concave(p)) => Pre(p)
      | (Concave(p), Convex) => Post(p)
      },
    sorts: {
      out: l_nib.sort,
      in_: [],
    },
  };
