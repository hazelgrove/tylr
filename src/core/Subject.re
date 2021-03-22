open Util;

[@deriving sexp]
type pointing('tile) = ListUtil.frame('tile);

[@deriving sexp]
type selecting('tile) = (
  (Direction.t, Selection.t(Unsorted.Tile.t)),
  ListUtil.frame(Selection.elem('tile)),
);

// TODO add side to focused selection
[@deriving sexp]
type restructuring('tile) = (
  Selection.t(Unsorted.Tile.t),
  ListUtil.frame(Selection.t(Unsorted.Tile.t)),
  ListUtil.frame(Either.t('tile, Selection.t(Unsorted.Tile.t))),
);

[@deriving sexp]
type t('tile) =
  | Pointing(pointing('tile))
  | Selecting(selecting('tile))
  | Restructuring(restructuring('tile));

let restructuring_of_pointing =
    (
      selection: Selection.t(Unsorted.Tile.t) as 's,
      selections: ListUtil.frame('s),
      pointing: pointing('tile),
    )
    : restructuring('tile) => (
  selection,
  selections,
  TupleUtil.map2(List.map(Either.l), pointing),
);
