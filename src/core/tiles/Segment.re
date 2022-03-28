open Util;

include Base.Segment;

let empty = [];
let cons = List.cons;
let concat = List.concat;
let fold_right = List.fold_right;

let of_shard = s => [Piece.Shard(s)];

let nibs = tiles =>
  switch (tiles, ListUtil.split_last_opt(tiles)) {
  | ([], _)
  | (_, None) => None
  | ([_first, ..._], Some((_, _last))) => failwith("todo Tiles.nibs")
  };

let shards = _ => failwith("todo shards");

let snoc = (tiles, tile) => tiles @ [tile];

let is_balanced = List.for_all(Piece.is_balanced);

let remove_matching = (_, _) => failwith("todo remove_matching");

let split_by_grout = _ => failwith("todo split_by_grout");

module Stack = Stack.Make(Orientation.R);
// TODO use direction parameter
let reassemble = (seg: t): t =>
  List.fold_right(Stack.push, seg, Stack.init) |> Stack.flatten;

// let rec remold = (
//   shard_ctx:
//   n_l: Nib.t, g_l: option(Grout.t),
//   seg: t,
//   g_r: option(Grout.t), n_r: Nib.t,
// ): t =>
//   fold_right(
//     (p, ((g_r, n_r), remolded)) =>
//       switch (p) {
//       | Grout(g) =>
//         // safe to drop g_r bc no-consecutive-grout invariant
//         ((Some(g), n_r), remolded)
//       | Shard(s) =>

//       | _ => failwith("todo")
//       },
//     seg,
//     ((g_r, n_r), empty),
//   );

// let rec remold =
//     (tiles: t, (l, r): Nibs.t, ctx: Shard.Ctx.t)
//     : list(t) => {
//   switch (tiles) {
//   | [] => [Nib.fits(l, r) ? [] : of_grout({nibs: (l, r)})]
//   | [Intact(tile), ...tiles] =>
//     open ListUtil.Syntax;
//     let* mold = Tile.(assignable_molds(~l, Intact.label(tile)));
//     let gs = Grouts.mk(Mold.nibs(mold), ;

//     let gs = Nibs.fits(l, tl) ? [] : of_grout({nibs: (l, tl)});
//     let+ tl = remold(tiles, (tr, r), ctx);
//     concat([gs, [Intact({...tile, mold}), tl]]);
//   | [Pieces(pieces), ...tiles] =>

//   }
// }
//   ListUtil.Syntax.(
//     tiles
//     |> List.fold_left(
//       ((remolded, (l, r), ctx), tile) =>
//         switch (tile) {
//         | Intact(tile) =>
//           let* mold = Tile.(assignable_molds(~l, label(tile)));

//         }
//       (Tiles.empty, nibs, ctx),
//     )
//   );

// let remold = (tiles: t, nibs: Nibs.t, ctx: Shard.Ctx.t): list(t) => {
//   open ListUtil.Syntax;
//   tiles
//   |> List.fold_left(
//     (((l, r), ctx), tile) =>
//       switch (tile) {
//       | Placeholder((Labeled(shard), [])) =>
//         let* nibs = Shard.assignable_nibs(~l, ctx, shard);
//         let shard = {...shard};
//         let ctx = Shard.Ctx.add(shard, ctx);

//       }
//     (nibs, ctx),
//   )
// };

// let sort_stacks = (tiles: t): (Sort.Stack.t, Sort.Stack.t) =>
//   tiles
//   |> List.fold_left(
//        ((popped, pushed) as stacks, tile: Tile.t) =>
//          switch (tile) {
//          | Intact(_) => stacks
//          | Pieces(ps) =>
//            open Sort.Stack;
//            let s = Tile.Pieces.sort(ps);
//            let (l, r) = Tile.Pieces.nibs(ps);
//            if (s == l.sort && s == r.sort) {
//              stacks;
//            } else if (s == l.sort) {
//              (popped, push(r.sort, pushed));
//            } else if (s == r.sort) {
//              switch (pop(pushed)) {
//              | None => (push(l.sort, popped), pushed)
//              | Some((_, pushed)) => (popped, pushed)
//              };
//            } else {
//              switch (pop(pushed)) {
//              | None => (push(l.sort, popped), push(r.sort, pushed))
//              | Some((_, pushed)) => (popped, push(r.sort, pushed))
//              };
//            };
//          },
//        Sort.Stack.(empty, empty),
//      );
