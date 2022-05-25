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
    | [Grout(_), ...affix] => shape(affix)
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
    | [Grout(_), ...affix] => sort(affix, s)
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
          let s' = tile.mold.sorts.out;
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
    let rec go = (s, rev_affix: t) =>
      switch (rev_affix: t) {
      | [] => []
      | [hd, ...tl] =>
        switch (hd) {
        | Tile(t) =>
          let hd = {
            let children =
              List.map(
                regrout((Nib.Shape.concave(), Nib.Shape.concave())),
                t.children,
              );
            Piece.Tile({...t, children});
          };
          let s = snd(O.orient(Mold.nibs(t.mold))).shape;
          [hd, ...go((l, r), tl)];
        | Shard(shard) =>
          let l = snd(shard.nibs).shape;
          [hd, ...regrout((l, r), tl)];
        | Whitespace(_) => [hd, ...regrout((l, r), tl)]
        | Grout(g) =>
          if (!Grout.fits_shape(g, l)) {
            regrout((l, r), tl);
          } else {
            let ((wss, gs), _, tl) = shape(tl, r);
            let ws = List.map(Piece.whitespace, List.concat(wss));
            let (g, l) =
              switch (Grout.merge([g, ...gs])) {
              | None => (None, l)
              | Some(g) => (Some(Piece.Grout(g)), snd(Grout.shapes(g)))
              };
            List.concat([Option.to_list(g), ws, regrout((l, r), tl)]);
          }
      };
    go(Nib.Shape.concave(), Segment.rev(affix));
  };

  let regrout = (affix: t) =>
    Segment.fold_right(
      (p: Piece.t, regrouted) =>
        switch (p) {
        | Grout(g) =>
          Grout.fits_shape(g, shape(regrouted))
            ? [p, ...regrouted] : regrouted
        | Shard(shard) =>
          let (n_far, _) = O.orient(shard.nibs);
          let s_far = n_far.shape;
          switch (regrouted) {
          | [Grout(g), ...tl] =>
            Grout.fits_shape(g, s_far) ? [p, ...regrouted] : [p, ...tl]
          | _ =>
            Nib.Shape.fits(s_far, shape(regrouted))
              ? [p, ...regrouted]
              : [p, Grout(Grout.mk_fits_shape(s_far)), ...regrouted]
          };
        | Tile(tile) =>
          let p =
            Piece.Tile({
              ...tile,
              children:
                List.map(
                  Segment.regrout((Nib.Shape.concave(), Nib.Shape.concave())),
                  tile.children,
                ),
            });
          let (n_far, _) = O.orient(Mold.nibs(tile.mold));
          let s_far = n_far.shape;
          switch (regrouted) {
          | [Grout(g), ...tl] =>
            Grout.fits_shape(g, s_far) ? [p, ...regrouted] : [p, ...tl]
          | _ =>
            Nib.Shape.fits(s_far, shape(regrouted))
              ? [p, ...regrouted]
              : [p, Grout(Grout.mk_fits_shape(s_far)), ...regrouted]
          };
        },
      affix,
      empty,
    );
};
