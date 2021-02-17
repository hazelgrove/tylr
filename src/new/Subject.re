open Util;

type pointing('tile) = ZZList.t(unit, 'tile) constraint 'tile = Tile.t(_);

type selecting('tile) =
  ZZList.t(
    (Direction.t, Selection.t(Unsorted.Tile.t)),
    Selection.elem('tile),
  );

type restructuring('tile) =
  ZZList.t(
    ZZList.t(Selection.t(Unsorted.Tile.t) as 's, 's),
    Either.t('tile, Selection.t('tile)),
  );

type t('tile) =
  | Pointing(pointing('tile))
  | Selecting(selecting('tile))
  | Restructuring(restructuring('tile));

let restructuring_of_pointing =
    (
      selections: ZZList.t(Selection.t(Unsorted.Tile.t) as 's, 's),
      (prefix, (), suffix): pointing('tile),
    )
    : restructuring('tile) => {
  let (prefix, suffix) =
    TupleUtil.map2(List.map(Either.l), (prefix, suffix));
  (prefix, selections, suffix);
};
