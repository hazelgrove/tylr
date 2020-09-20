module type S = {
  type tile;

  type zoperand;
  type zpreop;
  type zpostop;
  type zbinop;

  type nonrec t = Tile.t(zoperand, zpreop, zpostop, zbinop);
  type s = ZList.t(option(t), tile);

  let compare: (~compare_s: (s, s) => int, t, t) => int;

  let erase: (~erase_s: s => list(tile), t) => tile;

  let enter_from_left: tile => option(t);
  let enter_from_right: tile => option(t);

  let opt_map:
    (
      ~opt_map_s: ((list(tile), list(tile)) => option(s), s) => option(s),
      (list(tile), list(tile)) => option(s),
      t
    ) =>
    option(t);

  let remove:
    (~remove_s: (s, s) => option((list(tile), list(tile))), t, t) =>
    option((list(tile), tile));

  let restructure:
    (~restructure_s: (s, s, s) => option(s), t, t, t) => option(t);
};
