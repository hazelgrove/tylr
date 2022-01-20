open Sexplib.Std;
open Util;

[@deriving sexp]
type t = Aba.t(Tile.s, Shard.t);

let grow = (side: Direction.t, shard: Shard.t, segment: Segment.t): Segment.t =>
  switch (side) {
  | Left => Segment.cons_shard(shard, segment)
  | Right => Segment.snoc_shard(segment, shard)
  };

module Frame = {
  [@deriving sexp]
  type nonrec t = (t, t);

  let orient = (d: Direction.t, (prefix, suffix): t): t =>
    switch (d) {
    | Left => (prefix, suffix)
    | Right => (suffix, prefix)
    };
  let unorient = orient;

  let inner_nibs = (_frame, _outer_nibs) => failwith("todo");
};

let empty = ([], []);
let of_tiles = tiles => (tiles, []);

let concat: (t, t) => t = Aba.concat((@));

let trim = (segment: t) => {
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
  affixes: Frame.t,
  outer_nibs: Nibs.t,
): list((t, Tile.Frame.s)) => {
  let (l, r) = Frame.inner_nibs(affixes, outer_nibs);
  switch (segment) {
  | ([], []) => of_tiles(Tiles.glue(l, r))
  | ([], [(shard, tiles), ...tl]) =>
    open ListUtil.Syntax;
    let matching = Frame.shard_nibs(shard.tile_id, affixes);
    let* (ll, rr) as nibs = Shard.Form.nibs(~matching, shard.form);
    let glue_l = Tiles.(rev(glue(l, ll)));
    let+ (remolded_tl, glue_r) = {
      // TODO need to fix args to recursive call,
      // remember to update affixes passed in so that
      // any shard remoldings performed in this call
      // are taken into account for matching shards in tl
      let+ (tl, (glue_mid, glue_r)) = remold(shard_nibs, (tiles, tl), (rr, r));
      (concat([of_tiles(Tiles.rev(glue_mid)), tl]), glue_r)
    };
    let remolded = cons_shard({...shard, nibs}, remolded_tl);
    (remolded, (glue_l, glue_r));
  };
};

let choose = (_remoldings: list((t, Tile.Frame.s))): (t, Tile.Frame.s) =>
  failwith("todo");

let choose: list((t, Frame.t)) => (t, Frame.t) = failwith("todo");

let connect = (affixes: Frame.t, sort: Sort.t): list(Frame.t) => {
  let (l, r) = Frame.inner_nibs(affixes, sort);
  if (l.sort == r.sort) {
    Nib.of_sort(l.sort)
    |> List.map(mid => Frame.rerole((mid, mid), affixes, sort))
    |> List.concat
  } else {
    let mid_l = Nib.{sort: l.sort, orientation: Right};
    let mid_r = Nib.{sort: r.sort, orientation: Left};
    Frame.rerole((mid_l, mid_r), affixes, sort)
    |> List.map(((prefix, suffix)) => (prefix, [Tile.Sep, ...suffix]))
  };
};

let insert =
    (insertion: Segment.t, affixes: Frame.t, sort: Sort.t)
    : list((Segment.t, Frame.t)) => {
  open ListUtil.Syntax;
  let* remolded = remold(insertion, affixes, sort);
  switch (nibs(remolded)) {
  | None =>
    let+ affixes = connect(affixes, sort);
    (empty, affixes);
  | Some(inner_nibs) =>
    let+ affixes = Frame.rerole(inner_nibs, t, sort);

  };
};


let connect =
    (
      ~insert: option((Direction.t, t))=?,
      affixes: Frame.t,
      outer_nibs: Nibs.t,
    ) => {
  // TODO need to trim affixes
  let insertion =
    switch (insert) {
    | None =>
      let (l, r) = Frame.inner_nibs(affixes, outer_nibs);
      (empty, Segment.of_tiles(Tiles.glue(l, r)))
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