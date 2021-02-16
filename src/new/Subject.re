open Util;

type pointing('tile) = ZZList.t(unit, 'tile) constraint 'tile = Tile.t(_);

type selecting('tile) =
  ZZList.t(
    (Direction.t, list(Selection.t('tile))),
    Selection.elem('tile),
  );

type restructuring('tile) =
  ZZList.t(ZZList.t(Selection.t('tile) as 's, 's), Either.t('tile, 's));

type t('tile) =
  | Pointing(pointing('tile))
  | Selecting(selecting('tile))
  | Restructuring(restructuring('tile));
