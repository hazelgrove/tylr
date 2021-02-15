open Util;

module type S = {
  module Term: Term.S;
  module Tile: Tile.S with module Term := Term;
  module Frame: Frame.S with module Term := Term;

  module Subject: {
    // 1 | + (2 _ 3): {prefix: [1], z: (), suffix: [+, (2 _ 3)]}
    type pointing = ZList.t(unit, Tile.t);
    // 1 + [(] 2 _ 3 )
    // {prefix: [L(1), L(+)], z: (L, [(])), suffix: [L(2), L(_), L(3), R(`)`)]}
    type selecting =
      ZList.t(
        (Direction.t, Selection.t(Unsorted.Tile.t)),
        Either.t(Tile.t, Unsorted.Tessera.t),
      );
    // 1 + [( 2 _] 3 ) + 4
    // {prefix: [L(1), L(+)], z: (L, [[`(`]]), suffix: [L(2), L(_), L(3), R(`)`)]}
    type restructuring =
      ZList.t(
        (Direction.t, list(Selection.t(Unsorted.Tile.t) as 'selection)),
        Either.t(Tile.t, 'selection),
      );

    type t =
      | Pointing(pointing)
      | Selecting(selecting)
      | Restructuring(restructuring);
  };

  type t = (Subject.t, Frame.bidelimited);
};

module Make =
       (
         Term: Term.S,
         Tile: Tile.S with module Term := Term,
         Frame: Frame.S with module Term := Term,
       ) => {
  module Subject = {
    type pointing = ZList.t(unit, Tile.t);
    type selecting =
      ZList.t(
        (Direction.t, Selection.t(Unsorted.Tile.t)),
        Either.t(Tile.t, Unsorted.Tessera.t),
      );
    type restructuring =
      ZList.t(
        (Direction.t, list(Selection.t(Unsorted.Tile.t) as 'selection)),
        Either.t(Tile.t, 'selection),
      );
    type t =
      | Pointing(pointing)
      | Selecting(selecting)
      | Restructuring(restructuring);
  };

  type t = (Subject.t, Frame.bidelimited);
};

module Typ = Make(Term_typ, Tile_typ, Frame_typ);
module Pat = Make(Term_pat, Tile_pat, Frame_pat);
module Exp = Make(Term_exp, Tile_exp, Frame_exp);

type t =
  | Typ(Typ.t)
  | Pat(Pat.t)
  | Exp(Exp.t);
