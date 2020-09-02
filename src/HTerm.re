type t('operand, 'preop, 'postop, 'binop) = (
  Skel.t,
  Tiles.t('operand, 'preop, 'postop, 'binop),
);

type root('operand, 'preop, 'postop, 'binop) =
  | Operand('operand)
  | PreOp('preop, Skel.t)
  | PostOp(Skel.t, 'postop)
  | BinOp(Skel.t, 'binop, Skel.t);

let wrap = (operand: 'operand): t('operand, _, _, _) => (
  Skel.Operand(0),
  [Operand(operand)],
);

let get_nth_root = (n: int, (skel, tiles): t(_)): root(_) => {
  let rec go = (skel: Skel.t) =>
    switch (skel) {
    | Operand(m) =>
      assert(n == m);
      Operand(Tiles.get_operand(n, tiles));
    | PreOp(m, skel) =>
      assert(n >= m);
      n == m ? PreOp(Tiles.get_preop(n, tiles), skel) : go(skel);
    | PostOp(skel, m) =>
      assert(n <= m);
      n == m ? PostOp(skel, Tiles.get_postop(n, tiles)) : go(skel);
    | BinOp(skel1, m, skel2) =>
      if (n < m) {
        go(skel1);
      } else if (n > m) {
        go(skel2);
      } else {
        BinOp(skel1, Tiles.get_binop(n, tiles), skel2);
      }
    };
  go(skel);
};

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
