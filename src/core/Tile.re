open Sexplib.Std;

exception Ambiguous_molds;

module Id = {
  [@deriving sexp]
  type t = int;
  let compare = Int.compare;
};

module Map = {
  include Map.Make(Id);
};

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

module Mold = {
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
};

module Label = {
  [@deriving sexp]
  type t = list(Token.t);
};

[@deriving sexp]
type s = Util.Aba.t(Grouts.t, t)
and t = {
  id: Id.t,
  mold: Mold.t,
  substance: Util.Aba.t(Token.t, s),
};

let label = (tile: t) => Util.Aba.get_a(tile.substance);

let assignable_molds = (~l as _: option(Nib.t)=?, label: Label.t) => {
  open Mold;
  let s = Sorts.mk;
  switch (label) {
  | [t] when Token.is_num(t) => [mk_op(s(Exp))]
  | [t] when Token.is_var(t) => [mk_op(s(Pat)), mk_op(s(Exp))]
  | ["(", ")"] => [
      mk_op(s(~in_=[Pat], Pat)),
      mk_op(s(~in_=[Exp], Exp)),
    ]
  | ["λ", "{", "}"] => [mk_op(s(~in_=[Pat, Exp], Exp))]
  | ["!"] => [mk_post(Precedence.fact, s(Exp))]
  | ["[", "]"] => [mk_post(Precedence.ap, s(~in_=[Exp], Exp))]
  | ["*" | "/"] => [mk_bin(Precedence.mult, s(Exp))]
  | ["+" | "-"] => [mk_bin(Precedence.plus, s(Exp))]
  | [","] => [
      mk_bin(Precedence.prod, s(Pat)),
      mk_bin(Precedence.prod, s(Exp)),
    ]
  | ["?", ":"] => [mk_bin(Precedence.cond, s(~in_=[Exp], Exp))]
  | ["let", "=", "in"] => [
      mk_pre(Precedence.let_, s(~in_=[Pat, Exp], Exp)),
    ]
  | _ => []
  };
};

let default_mold =
    (_form: Label.t, _sibling: Sort.t, _ancestor: Sort.t): Mold.t =>
  failwith("todo Tile.default_mold");

let nibs: (~index: int=?, Mold.t) => Nibs.t =
  (~index as _=?, _) => failwith("todo Tile.nibs");

let reshape = (tile: t) =>
  assignable_molds(label(tile))
  |> List.filter((mold: Mold.t) => mold.sorts == tile.mold.sorts)
  |> List.map(mold => {...tile, mold});

module Frame = {
  [@deriving sexp]
  type t = {
    id: Id.t,
    mold: Mold.t,
    substance: Util.Aba.Frame.B.t(Token.t, s),
  };

  [@deriving sexp]
  type step = int;

  let step = (frame: t): step => {
    let (prefix, _) = frame.substance;
    List.length(Util.Aba.get_b(prefix));
  };

  let label = _ => failwith("todo Tile.Frame.label");

  let sort = (frame: t): Sort.t =>
    List.nth(frame.mold.sorts.in_, step(frame));
};
