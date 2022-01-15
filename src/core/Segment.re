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

let trim = (segment: Segment.t) => {
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
  segment: t,
  affixes: Segment.Frame.t,
  outer_nibs: Nibs.t,
): list((t, Tile.Frame.s)) => {
  let (l, r) = Frame.inner_nibs(affixes, outer_nibs);
  switch (segment) {
  | ([], []) => Segment.of_tiles(Tiles.glue(l, r))
  | ([], [(shard, tiles), ...tl]) =>
    open List.Syntax;
    let matching = Frame.shard_nibs(shard.tile_id, affixes);
    let* (ll, rr) as nibs = Shard.Form.nibs(~matching, shard.form);
    let glue_l = Tiles.(rev(glue(l, ll)));
    let+ (remolded_tl, glue_r) = {
      let+ (tl, (glue_mid, glue_r)) = remold(shard_nibs, (tiles, tl), (rr, r));
      (concat([of_tiles(Tiles.rev(glue_mid)), tl]), glue_r)
    };
    let remolded = cons_shard({...shard, nibs}, remolded_tl);
    (remolded, (glue_l, glue_r));
  };
};

let choose = (_remoldings: list((t, Tile.Frame.s))): (t, Tile.Frame.s) =>
  failwith("todo");

let connect =
    (
      ~insert: (Direction.t, Segment.t)=?,
      affixes: Segment.Frame.t,
      (l, r) as outer_nibs: Nibs.t,
    ) => {
  let insertion =
    switch (insert) {
    | None => (empty, Segment.of_tiles(Tiles.glue(l, r)))
    | Some((d, insertion)) =>
      let remoldings = remold(insertion, affixes, outer_nibs);
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