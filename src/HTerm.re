type t('operand, 'preop, 'postop, 'binop) = (
  Skel.t,
  Tiles.t('operand, 'preop, 'postop, 'binop),
);

let wrap = (operand: 'operand): t('operand, _, _, _) => (
  Skel.Operand(0),
  [Operand(operand)],
);

let put_nth = (n: int, tile: Tile.t(_), (skel, tiles): t(_)): t(_) => (
  skel,
  Tiles.put_nth(n, tile, tiles),
);

let set_hole_status =
    (
      ~set_tile: (HoleStatus.t, Tile.t(_)) => Tile.t(_),
      status: HoleStatus.t,
      (skel, tiles): t(_),
    )
    : t(_) => {
  let set_root =
    switch (skel) {
    | Operand(n)
    | PreOp(n, _)
    | PostOp(_, n)
    | BinOp(_, n, _) => Tiles.map_nth(n, set_tile(status))
    };
  (skel, set_root(tiles));
};
