open Util;

module Make = (O: Orientation.S) => {
  // module Stack = Stack.Make(O);
  // module Match = Tile.Match.Make(O);

  [@deriving show]
  type t = Segment.t;

  let empty = Segment.empty;

  let incomplete_tiles = Segment.incomplete_tiles;
  let split_by_matching = Segment.split_by_matching;
  let contains_matching = Segment.contains_matching;

  let reassemble: t => t = Segment.reassemble(~flip=O.d == Left);

  let rec shape = (affix: t) =>
    switch (affix) {
    | [] => Nib.Shape.concave()
    | [Whitespace(_) | Grout(_), ...affix] => shape(affix)
    | [Tile(t), ..._] => fst(Tile.shapes(~flip=O.d == Left, t))
    };

  let rec sort = (affix: t, s: Sort.t) =>
    switch (affix) {
    | [] => s
    | [Whitespace(_) | Grout(_), ...affix] => sort(affix, s)
    | [Tile(t), ..._] => fst(Tile.nibs(~flip=O.d == Left, t)).sort
    };

  let push = (p, affix: t) => reassemble(Segment.cons(p, affix));

  let pop = (affix: t): option((Piece.t, t)) =>
    switch (affix) {
    | [] => None
    | [p, ...affix] =>
      let (p, affix') = Piece.pop(Direction.toggle(O.d), p);
      Some((p, Segment.concat([affix', affix])));
    };

  let remold = Segment.remold;

  // let sort_rank = (affix: t, s: Sort.t) =>
  //   Segment.fold_right(
  //     (p: Piece.t, (s, rank)) =>
  //       switch (p) {
  //       | Whitespace(_)
  //       | Grout(_) => (s, rank)
  //       | Shard(shard) =>
  //         let (n_far, n_near) = O.orient(shard.nibs);
  //         let (s_far, s_near) = (n_far.sort, n_near.sort);
  //         (s_near, rank + Bool.to_int(!Sort.consistent(s_far, s)));
  //       | Tile(tile) =>
  //         let s' = tile.mold.out;
  //         let children_ranks =
  //           Tile.sorted_children(tile)
  //           |> List.map(((s, child)) => Segment.sort_rank(child, (s, s)))
  //           |> List.fold_left((+), 0);
  //         let rank' =
  //           rank + children_ranks + Bool.to_int(!Sort.consistent(s, s'));
  //         (s', rank');
  //       },
  //     affix,
  //     (s, 0),
  //   )
  //   |> snd;
  let sort_rank = (_, _) => failwith("todo sort_rank");

  let shape_rank = affix =>
    snd(
      Segment.shape_rank_tl(~flip=O.d == Left, affix, Nib.Shape.concave()),
    );

  let regrout = affix =>
    Segment.regrout_tl(~flip=O.d == Left, affix, Nib.Shape.concave());
};
