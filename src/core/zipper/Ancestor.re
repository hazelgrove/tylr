// [@deriving sexp]
// type step = int;

module Intact = {
  [@deriving sexp]
  type t = {
    id: Id.t,
    mold: Mold.t,
    substance: Util.Aba.Frame.B.t(Token.t, Tiles.t),
  };

  // let step = (frame: t): step => {
  //   let (prefix, _) = frame.substance;
  //   List.length(Util.Aba.get_b(prefix));
  // };

  let sort = (frame: t): Sort.t =>
    List.nth(frame.mold.sorts.in_, step(frame));
  // let disassemble = (frame: t): (s, s) =>
  //   switch (frame) {
  //   | Placeholder({substance: (prefix, suffix)}) =>
  //   }

  let label = _ => failwith("todo label");
};

[@deriving sexp]
type t =
  | Pieces(Util.Aba.Frame.B.t(Piece.t, Tiles.t))
  | Intact(Intact.t);

// [@deriving sexp]
// type t = (Tile.Frame.t, Siblings.t);
