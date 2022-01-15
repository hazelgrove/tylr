open Sexplib.Std;
open Util;
open OptUtil.Syntax;

[@deriving sexp]
type t = Aba.t(Tile.s, Shard.t);

module Frame = {
  [@deriving sexp]
  type nonrec t = (t, t);

  let nibs = (_frame, _nibs) => failwith("todo");
};

let empty = ([], []);
let of_tiles = tiles => (tiles, []);

let concat: (t, t) => t = Aba.concat((@));

let glue = (l: Nib.t, r: Nib.t): Tile.s => {
  let hole_nibs =
  if (l == r) {
    []
  } else if (l.sort == r.sort) {
    [(l, r)]
  } else {
    let nibs =
    switch (l.orientation, r.orientation) {
    | (Left, Right) =>
      let (l', r') = (Nib.toggle(l), Nib.toggle(r));
      [(l, l'), (l', r'), (r', r)];
    | (Left, Left) =>
      let l' = Nib.toggle(l);
      [(l, l'), (l', r)];
    | (Right, Right) =>
      let r' = Nib.toggle(r);
      [(l, r'), (r', r)];
    | (Right, Left) => [(l, r)]
    };
  };
  Tile.s_of_nibs(hole_nibs);
};

let trim_end_holes = (segment: Segment.t) => {
  let trim_l =
    fun
    | [hd, ...tl] when Piece.is_hole(hd) => tl
    | segment => segment;
  let trim_r = segment =>
    switch (ListUtil.split_last_opt(segment)) {
    | Some((leading, last)) when Piece.is_hole(last) => leading
    | _ => segment
    };
  trim_r(trim_l(segment));
};

let rec remold = (
  shard_nibs: list((Tile.Id.t, list((Shard.Index.t, Nibs.t)))),
  segment: t,
  (l, r): Nibs.t,
): list((t, Tile.Frame.s)) =>
  switch (segment) {
  | ([], []) => Segment.of_tiles(glue(l, r))
  | ([], [(shard, tiles), ...tl]) =>
    let matching =
      switch (List.assoc_opt(shard.tile_id, shard_nibs)) {
      | None => []
      | Some(matching) => matching
      };
    open List.Syntax;
    let* (ll, rr) as nibs = Shard.Form.nibs(~matching, shard.form);
    let glue_l = Tiles.rev(glue(l, ll));
    let+ (remolded_tl, glue_r) = {
      let+ (tl, (glue_mid, glue_r)) = remold(shard_nibs, (tiles, tl), (rr, r));
      (concat([of_tiles(Tiles.rev(glue_mid)), tl]), glue_r)
    };
    let remolded = cons_shard({...shard, nibs}, remolded_tl);
    (remolded, (glue_l, glue_r));
  };

let choose = (_remoldings: list((t, Tile.Frame.s))): (t, Tile.Frame.s) =>
  failwith("todo");

let connect =
    (
      ~insert: (Direction.t, Segment.t)=?,
      affixes: Segment.Frame.t,
      outer_nibs: Nibs.t,
    ) => {
  let (l, r) = Frame.nibs(affixes, nibs);
  let insertion =
    switch (insert) {
    | None => (empty, Segment.of_tiles(glue(l, r)))
    | Some((d, insertion)) =>
      let shard_nibs = Frame.shard_nibs(affixes);
      let remoldings = remold(shard_nibs, insertion, (l, r));
      let (insertion, (glue_l, glue_r)) = choose(remoldings);
      let (glue_l, glue_r) = (of_tiles(glue_l), of_tiles(glue_r));
      switch (d) {
      | Left =>
        (glue_l, concat([insertion, glue_r]))
      | Right =>
        (concat([rev(insertion), glue_l]), glue_r)
      };
    };
  Frame.append(insertion, affixes);
};