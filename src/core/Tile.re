open Sexplib.Std;

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
    | Pre(Precedence.t)
    | Post(Precedence.t)
    | Bin(Precedence.t);

  let precedence: t => Precedence.t =
    _ => failwith("todo Tile.Shape.precedence");
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
    shape: Shape.t,
    sorts: Sorts.t,
  };
  let nibs: (~index: int=?, t) => Nibs.t =
    (~index as _=?, _) => failwith("todo Tile.Mold.nibs");
};

module Form = {
  [@deriving sexp]
  type t = list(Token.t);

  let molds = (~l as _: option(Nib.t)=?) =>
    Mold.(
      fun
      | [t] when Token.is_num(t) => [{shape: Op, sorts: Sorts.mk(Exp)}]
      | [t] when Token.is_var(t) => [
          {shape: Op, sorts: Sorts.mk(Pat)},
          {shape: Op, sorts: Sorts.mk(Exp)},
        ]
      | ["(", ")"] => [
          {shape: Op, sorts: Sorts.mk(~in_=[Pat], Pat)},
          {shape: Op, sorts: Sorts.mk(~in_=[Exp], Exp)},
        ]
      | ["λ", "{", "}"] => [
          {shape: Op, sorts: Sorts.mk(~in_=[Pat, Exp], Exp)},
        ]
      | ["!"] => [{shape: Post(1), sorts: Sorts.mk(Exp)}]
      | ["[", "]"] => [
          {shape: Post(2), sorts: Sorts.mk(~in_=[Exp], Exp)},
        ]
      | ["*" | "/"] => [{shape: Bin(3), sorts: Sorts.mk(Exp)}]
      | ["+" | "-"] => [{shape: Bin(4), sorts: Sorts.mk(Exp)}]
      | [","] => [
          {shape: Bin(5), sorts: Sorts.mk(Exp)},
          {shape: Bin(5), sorts: Sorts.mk(Pat)},
        ]
      | ["?", ":"] => [{shape: Bin(6), sorts: Sorts.mk(~in_=[Exp], Exp)}]
      | ["let", "=", "in"] => [
          {shape: Pre(9), sorts: Sorts.mk(~in_=[Pat, Exp], Exp)},
        ]
      | _ => []
    );
};

module Placeholder = {
  [@deriving sexp]
  type t =
    | Sep(Nibs.t)
    | Hole(Sort.t);
};

[@deriving sexp]
type s = Util.Aba.t(list(Placeholder.t), t)
and t = {
  id: Id.t,
  mold: Mold.t,
  tokens: Util.Aba.t(Token.t, s),
};

let form = tile => Util.Aba.get_as(tile.tokens);

module Frame = {
  [@deriving sexp]
  type t = {
    id: Id.t,
    mold: Mold.t,
    tokens: Util.Aba.Frame.b(Token.t, s),
  };
};
