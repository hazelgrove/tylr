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

let outer_nibs = ({shape, sorts}: t): Nibs.t => {
  let sort = sorts.out;
  let convex: Nib.t = {shape: Convex, sort};
  let concave: Precedence.t => Nib.t = p => {shape: Concave(p), sort};
  switch (shape) {
  | Op => (convex, convex)
  | Pre(p) => (convex, concave(p))
  | Post(p) => (concave(p), convex)
  | Bin(p) => (concave(p), concave(p))
  };
};

let nibs = (~index=?, mold: t): Nibs.t => {
  let (l, r) = outer_nibs(mold);
  switch (index) {
  | None => (l, r)
  | Some(i) =>
    let in_ = mold.sorts.in_;
    let l =
      i == 0 ? l : Nib.{shape: Shape.concave(), sort: List.nth(in_, i - 1)};
    let r =
      i == List.length(in_)
        ? r : Nib.{shape: Shape.concave(), sort: List.nth(in_, i)};
    (l, r);
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
      // TODO(d): revisit this when reformulating molds
      switch (g) {
      | (Convex, Convex) => Op
      | (Convex, Concave(p)) => Pre(p)
      | (Concave(p), Convex) => Post(p)
      | (Concave(p), Concave(_)) => Bin(p)
      },
    sorts: {
      out: sort,
      in_: [],
    },
  };

let of_shard =
    // TODO(andrew): dont do this?
    (nibs: Nibs.t, n: int, label: list(string)): t => {
  let shape: Shape.t =
    switch (nibs) {
    | (l_nib, _) when n == 0 =>
      switch (l_nib.shape) {
      | Convex => Pre(Precedence.min)
      | Concave(_) => Bin(Precedence.min)
      }
    | (_, r_nib) when n == List.length(label) - 1 =>
      switch (r_nib.shape) {
      | Convex => Post(Precedence.min)
      | Concave(_) => Bin(Precedence.min)
      }
    | _ => Bin(Precedence.min)
    };
  {
    sorts: {
      out: Exp,
      in_: [],
    },
    shape,
  };
};
