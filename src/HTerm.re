type t('operand, 'preop, 'postop, 'binop) = (
  Skel.t,
  list(Tile.t('operand, 'preop, 'postop, 'binop)),
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
  let tile = List.nth(tiles, n);
  let rec go = (skel: Skel.t) => {
    switch (skel) {
    | Operand(m) =>
      assert(n == m);
      Operand(Tile.get_operand(tile));
    | PreOp(m, skel) =>
      assert(n >= m);
      n == m ? PreOp(Tile.get_preop(tile), skel) : go(skel);
    | PostOp(skel, m) =>
      assert(n <= m);
      n == m ? PostOp(skel, Tile.get_postop(tile)) : go(skel);
    | BinOp(skel1, m, skel2) =>
      if (n < m) {
        go(skel1);
      } else if (n > m) {
        go(skel2);
      } else {
        BinOp(skel1, Tile.get_binop(tile), skel2);
      }
    };
  };
  go(skel);
};

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
    | BinOp(_, n, _) => ListUtil.map_nth(n, set_tile(status))
    };
  (skel, set_root(tiles));
};
