include Base.Shard;

module Label = {
  include Label;

  let token = ((n, lbl)) => List.nth(lbl, n);

  let is_next = ((n_l, lbl_l), (n_r, lbl_r)) =>
    n_l + 1 == n_r && lbl_l == lbl_r;
};

let mk = (label: Label.t, nibs: Nibs.t) => {label, nibs};

let mk_s = (label: Base.Tile.Label.t, mold: Mold.t): list(t) =>
  label |> List.mapi((i, _) => mk((i, label), Mold.nibs(~index=i, mold)));

let to_piece = s => Base.Piece.Shard(s);

let tile_label = s => snd(s.label);

// TODO generalize direction
let is_next = (l: t, r: t) => Label.is_next(l.label, r.label);
