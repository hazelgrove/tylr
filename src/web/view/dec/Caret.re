open Tylr_core;

module Shape = {
  // None means within token
  // Some((d, tip)) means on d side of token
  type t =
    | Inner
    | Outer(Dir.t, Tip.t);
};
module Profile = {
  type t = {
    pos: Loc.t,
    hand: Caret.Hand.t,
    shape: Shape.t,
  };
  // let mk = (~state: Layout.State.t, cell: Cell.t) => {
  // }
};
