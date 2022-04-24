open Util;

module Make = (O: Orientation.S) => {
  module Stack = Stack.Make(O);

  [@deriving show]
  type t = Segment.t;

  let empty = Segment.empty;

  let match = affix => List.fold_right(Stack.push, affix, Stack.init);

  let reassemble = (affix: t): t => Stack.flatten(match(affix));

  let split_nearest_grouts = (_: Direction.t, _: t) =>
    failwith("todo split_nearest_grouts");

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

  let sort_rank = (affix: t, s: Sort.t) =>
    Segment.fold_right(
      (p: Piece.t, (s, rank)) =>
        switch (p) {
        | Grout(_) => (s, rank)
        | Shard(shard) =>
          let (n_far, n_near) = Nibs.orient(O.d, shard.nibs);
          let (s_far, s_near) = (n_far.sort, n_near.sort);
          (s_near, rank + Bool.to_int(s_far != s));
        | Tile(tile) =>
          let s' = tile.mold.sorts.out;
          let children_ranks =
            Tile.sorted_children(tile)
            |> List.map(((s, child)) => Segment.sort_rank(child, (s, s)))
            |> List.fold_left((+), 0);
          let rank' = rank + children_ranks + Bool.to_int(s != s');
          (s', rank');
        },
      affix,
      (s, 0),
    )
    |> snd;

  // let sort_stacks = (d: Direction.t, affix: t): (Sort.Stack.t, Sort.Stack.t) => {
  //   let (pushed, popped) =
  //     Tiles.fold_right(
  //       (tile: Tile.t, (pushed, popped)) =>
  //         switch (tile) {
  //         | Intact(intact) => stack
  //         | Pieces(pieces) =>
  //           open Sort.Stack;
  //           let s = Pieces.sort(ps);
  //           let (front, back) =
  //             Nibs.orient(Direction.toggle(d), Pieces.nibs(ps));
  //           if (s == front.sort && s == back.sort) {
  //             stacks;
  //           } else if (s == front.sort) {
  //             (push(s, pushed), popped);
  //           } else if (s == back.sort) {
  //             switch (pop(pushed)) {
  //             | None => (pushed, push(s, popped))
  //             | Some((_, pushed)) => (pushed, popped)
  //             };
  //           } else {
  //             switch (pop(pushed)) {
  //             | None => (push(s, pushed), push(s, popped))
  //             | Some((_, pushed)) => (push(s, pushed), popped)
  //             };
  //           };
  //         },
  //       affix,
  //       Sort.Stack.(empty, empty),
  //     );
  //   (pushed, Sort.Stack.rev(popped));
  // };
  // let sort_stack = (d, affix) => fst(sort_stacks(d, affix));

  // let split_nearest_grouts = (d: Direction.t, affix: t) =>
  //   switch (affix) {
  //   | []
  //   | [Intact(_), ..._] => ([], affix)
  //   | [Pieces(pieces), ...affix'] =>
  //     let pieces = d == Left ? Aba.rev(pieces) : pieces;
  //     switch (pieces) {
  //     | (Shard(_), _) => ([], affix)
  //     | (Grout(g), []) =>
  //       let (gs, affix) = split_nearest_grouts(affix');
  //       ([g, ...gs], affix);
  //     | (Grout(g), [_, ..._] as tl) =>
  //       let affix'' =
  //         tl
  //         |> List.map(((tiles, piece)) =>

  //         )

  //       ([g], )

  //       let gs =
  //         switch (tl) {
  //         |
  //         }
  //     }

  //   | [Pieces((Shard(_), _)), ..._]
  //   | [Pieces((Grout(g), tl)), ...affix] =>
  //     let gs =
  //       switch (tl) {
  //       | [] => split_nearest_grouts(d)
  //       }
  //   }

  // type nonrec hd = hd;
  // type nonrec tl = tl;
  // let near_nib_tl = (d: Direction.t, tl: tl, far_nib: Nib.t) =>
  //   switch (tl) {
  //   | [] => far_nib
  //   | [(tile, _), ..._] =>
  //     Direction.(choose(toggle(d), Tile.nibs(tile.mold)))
  //   };
  // let near_nib = (d: Direction.t, (hd, tl): t, far_nib: Nib.t) => (
  //   hd,
  //   near_nib_tl(d, tl, far_nib),
  // );
  // let split_hd = (d, affix) =>
  //   switch (affix) {
  //   | [] => None
  //   | [hd, ...tl] =>
  //     let Tile.disassemble(hd))
  //     switch (Tile.disassemble_hd(hd)) {
  //     | [] => Some((hd, tl))
  //     |  =>
  //     }
  //   }
  // let split_hd = split_hd;
  // let reshape_tl = (d: Direction.t, tl: tl, far_nib: Nib.t): list(tl) => {
  //   let fold = ((tile, ps), (tl: tl, k: unit => list(tl))) => {
  //     let tl = [(tile, ps), ...tl];
  //     let k = () =>
  //       switch (Tile.reshape(tile)) {
  //       | [_] => [tl] // short-circuit reshaping when only one option
  //       | reshapings =>
  //         open ListUtil.Syntax;
  //         let* reshaped_tile = reshapings;
  //         let+ reshaped_tl = k();
  //         let adjusted_ps = {
  //           let far_nib = near_nib_tl(d, reshaped_tl, far_nib);
  //           let near_nib = Direction.choose(d, Tile.nibs(tile.mold));
  //           adjust_placeholders(d, ps, (near_nib, far_nib));
  //         };
  //         [(reshaped_tile, adjusted_ps), ...reshaped_tl];
  //       };
  //     (tl, k);
  //   };
  //   let (_, k) = List.fold_right(fold, tl, ([], () => [[]]));
  //   k();
  // };
  // let reshape = (d: Direction.t, (hd, tl): t, far_nib: Nib.t): list(t) =>
  //   reshape_tl(d, tl, far_nib) |> List.map(tl => (hd, tl));

  let split_hd = _ => failwith("todo split_hd");

  let near_nib = (_, _) => failwith("near_nib todo");
  let reshape = (_, _) => failwith("reshape todo");
};
