open Util;

module Make = (O: Orientation.S) => {
  // module Stack = Stack.Make(O);
  module Match = Tile.Match.Make(O);

  [@deriving show]
  type t = Segment.t;

  let empty = Segment.empty;

  let shards = Segment.shards;
  let split_by_matching_shard = Segment.split_by_matching_shard;

  let rec reassemble = (affix: t): t =>
    switch (shards(affix)) {
    | [] => affix
    | [s, ..._] =>
      switch (Aba.trim(split_by_matching_shard(s.tile_id, affix))) {
      | None => affix
      | Some((affix_inner, match, affix_outer)) =>
        let affix_m =
          switch (Match.complete(match)) {
          | None => Match.join(match)
          | Some(t) =>
            let children = List.map(Segment.reassemble, t.children);
            [Tile.to_piece({...t, children})];
          };
        Segment.concat([affix_inner, affix_m, reassemble(affix_outer)]);
      }
    };

  let rec shape = (affix: t) =>
    switch (affix) {
    | [] => Nib.Shape.concave()
    | [Whitespace(_) | Grout(_), ...affix] => shape(affix)
    | [Shard(s), ..._] =>
      let (_, near) = O.orient(s.nibs);
      near.shape;
    | [Tile(t), ..._] =>
      let (_, near) = O.orient(Mold.nibs(t.mold));
      near.shape;
    };

  let rec sort = (affix: t, s: Sort.t) =>
    switch (affix) {
    | [] => s
    | [Whitespace(_) | Grout(_), ...affix] => sort(affix, s)
    | [Shard(s), ..._] =>
      let (_, near) = O.orient(s.nibs);
      near.sort;
    | [Tile(t), ..._] =>
      let (_, near) = O.orient(Mold.nibs(t.mold));
      near.sort;
    };

  let push = (p, affix: t) => reassemble(Segment.cons(p, affix));

  let pop = (~balanced: bool, affix: t): option((Piece.t, t)) =>
    switch (affix) {
    | [] => None
    | [p, ...affix] =>
      if (balanced) {
        Piece.is_balanced(p) ? Some((p, affix)) : None;
      } else {
        let (p, affix') = Piece.pop(Direction.toggle(O.d), p);
        Some((p, Segment.concat([affix', affix])));
      }
    };

  let shards = Segment.shards;

  let contains_matching = Segment.contains_matching;

  let remold = Segment.remold;

  let sort_rank = (affix: t, s: Sort.t) =>
    Segment.fold_right(
      (p: Piece.t, (s, rank)) =>
        switch (p) {
        | Whitespace(_)
        | Grout(_) => (s, rank)
        | Shard(shard) =>
          let (n_far, n_near) = O.orient(shard.nibs);
          let (s_far, s_near) = (n_far.sort, n_near.sort);
          (s_near, rank + Bool.to_int(!Sort.consistent(s_far, s)));
        | Tile(tile) =>
          let s' = tile.mold.out;
          let children_ranks =
            Tile.sorted_children(tile)
            |> List.map(((s, child)) => Segment.sort_rank(child, (s, s)))
            |> List.fold_left((+), 0);
          let rank' =
            rank + children_ranks + Bool.to_int(!Sort.consistent(s, s'));
          (s', rank');
        },
      affix,
      (s, 0),
    )
    |> snd;

  let shape_rank = affix =>
    Segment.fold_right(
      (p: Piece.t, (s, rank)) =>
        switch (p) {
        | Whitespace(_)
        | Grout(_) => (s, rank)
        | Shard(shard) =>
          let (n_far, n_near) = O.orient(shard.nibs);
          let (s_far, s_near) = (n_far.shape, n_near.shape);
          (s_near, rank + Bool.to_int(!Nib.Shape.fits(s_far, s)));
        | Tile(tile) =>
          let (n_far, n_near) = O.orient(Mold.nibs(tile.mold));
          let (s_far, s_near) = (n_far.shape, n_near.shape);
          let children_ranks =
            tile.children
            |> List.map(child =>
                 Segment.shape_rank(child, Nib.Shape.(concave(), concave()))
               )
            |> List.fold_left((+), 0);
          let rank' =
            rank + children_ranks + Bool.to_int(!Nib.Shape.fits(s_far, s));
          (s_near, rank');
        },
      affix,
      (Nib.Shape.concave(), 0),
    )
    |> snd;

  let regrout = (affix: t) => {
    let r = Nib.Shape.concave();
    switch (O.d) {
    | Left =>
      let (trim, s, tl) = Segment.regrout_tl(Segment.flip_nibs(affix), r);
      (trim, s, Segment.flip_nibs(tl));
    | Right => Segment.regrout_tl(affix, r)
    };
  };
};
