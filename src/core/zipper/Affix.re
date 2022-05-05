open Util;

module Make = (O: Orientation.S) => {
  module Stack = Stack.Make(O);

  [@deriving show]
  type t = Segment.t;

  let empty = Segment.empty;

  let match = affix => List.fold_right(Stack.push, affix, Stack.init);

  let reassemble = (affix: t): t => Stack.flatten(match(affix));

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

  let remold = Segment.remold;

  let sort_rank = (affix: t, s: Sort.t) =>
    Segment.fold_right(
      (p: Piece.t, (s, rank)) =>
        switch (p) {
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

  let regrout = (affix: t) =>
    Segment.fold_right(
      (p: Piece.t, regrouted) =>
        switch (p) {
        | Grout(g) =>
          Grout.fits(g, shape(regrouted)) ? [p, ...regrouted] : regrouted
        | Shard(shard) =>
          let (n_far, _) = O.orient(shard.nibs);
          let s_far = n_far.shape;
          switch (regrouted) {
          | [Grout(g), ...tl] =>
            Grout.fits(g, s_far) ? [p, ...regrouted] : [p, ...tl]
          | _ =>
            Nib.Shape.fits(s_far, shape(regrouted))
              ? [p, ...regrouted]
              : [p, Grout(Grout.mk_fits(s_far)), ...regrouted]
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
            Grout.fits(g, s_far) ? [p, ...regrouted] : [p, ...tl]
          | _ =>
            Nib.Shape.fits(s_far, shape(regrouted))
              ? [p, ...regrouted]
              : [p, Grout(Grout.mk_fits(s_far)), ...regrouted]
          };
        },
      affix,
      empty,
    );
};
