// open Util;
// let wrap = ((tiles_near, tiles_far), s: Aba.t(Shard.t, Tiles.t)): Segment.t =>
//   s |> Baba.cons(tiles_near) |> Fun.flip(Aba.snoc, tiles_far);
// let disassemble_tile = (tile: Tile.t): Segment.t =>
//   tile.substance
//   |> Util.Aba.mapi_a((index, _) => Shard.of_tile(index, tile))
//   |> wrap((Tiles.empty, Tiles.empty));
// /**
//  * hd of output is guaranteed to be intact
//  */
// let split_by_matching_shards =
//     ((tiles, rest): Segment.t)
//     : (Tiles.t, option((Aba.t(Shard.t, Segment.t), Segment.t))) => {
//   let split =
//     switch (rest) {
//     | [] => None
//     | [(hd_shard, tiles'), ...rest] =>
//       let (tl, rest) =
//         (tiles', rest)
//         |> Aba.split(s =>
//              Shard.id(s) == Shard.id(hd_shard) ? Some(s) : None
//            )
//         |> Aba.split_last;
//       Some(((hd_shard, tl), rest));
//     };
//   (tiles, split);
// };
// let join = (split: Aba.t(Shard.t, Segment.t)): Segment.t =>
//   split
//   |> Aba.map_to_list(s => Segment.of_pieces([Shard(s)]), Fun.id)
//   |> Segment.concat;
// let unique_mold = shards =>
//   switch (Shard.assignable_molds(shards)) {
//   | [] => raise(Shard.Inconsistent_nibs)
//   | [_, _, ..._] => raise(Tile.Ambiguous_molds)
//   | [mold] => mold
//   };
// let reassemble_affix =
//     (side: Direction.t, affix: Tiles.Affix.t): Tiles.Affix.t =>
//   switch (side) {
//   | Left => Tiles.(rev(reassemble_tiles(rev(affix))))
//   | Right => reassemble_tiles(affix)
//   };
// let disassemble_ancestor = ((tile, tiles): Ancestor.t): Siblings.t => {
//   let step = Tile.Frame.step(tile);
//   let (tile_pre, tile_suf) = tile.substance;
//   let (tiles_pre, tiles_suf) = tiles;
//   let prefix =
//     tile_pre
//     |> Aba.mapi_a((i, _) => Shard.of_tile_frame(step - i, tile))
//     |> wrap((Tiles.empty, tiles_pre));
//   let suffix =
//     tile_suf
//     |> Aba.mapi_a((i, _) => Shard.of_tile_frame(step + 1 + i, tile))
//     |> wrap((Tiles.empty, tiles_suf));
//   (prefix, suffix);
// };
// let rec reassemble_relatives =
//         (siblings: Siblings.t, ancestors: Ancestors.t)
//         : (Siblings.t, Ancestors.t) => {
//   let map2 = TupleUtil.map2;
//   switch (map2(split_by_matching_shards, siblings)) {
//   | ((_, None), _)
//   | (_, (_, None)) => (siblings, ancestors)
//   | (
//       (tiles_pre, Some((tile_split_pre, rest_pre))),
//       (tiles_suf, Some((tile_split_suf, rest_suf))),
//     ) =>
//     let tile_split = (
//       Aba.map_b(reassemble_affix(Left), tile_split_pre),
//       Aba.map_b(reassemble_affix(Right), tile_split_suf),
//     );
//     let (rest, ancestors) =
//       reassemble_relatives((rest_pre, rest_suf), ancestors);
//     let (remaining_siblings, attempted_tile_assembly) = {
//       let (id, label) = Aba.hd(tile_split_pre).tile;
//       let (shards_pre, shards_suf) = map2(Aba.get_a, tile_split);
//       if (List.length(shards_pre)
//           + List.length(shards_suf) < List.length(label)) {
//         let siblings = Segment.Frame.concat([map2(join, tile_split), rest]);
//         (siblings, ancestors);
//       } else {
//         let mold = unique_mold(ListFrame.to_list((shards_pre, shards_suf)));
//         let substance =
//           tile_split
//           |> map2(Aba.map_a(Shard.label))
//           |> map2(Aba.map_b(Aba.hd));
//         let ancestor = (
//           Tile.Frame.{id, mold, substance},
//           map2(Aba.hd, rest),
//         );
//         (Segment.Frame.empty, [ancestor, ...ancestors]);
//       };
//     };
//     (
//       Segment.Frame.(
//         concat([of_tiles((tiles_pre, tiles_suf)), remaining_siblings])
//       ),
//       attempted_tile_assembly,
//     );
//   };
// };
// type itile = (int, Tile.t);
// let associate = (tiles: Tiles.t): Skel.t => {
//   let push_output =
//       ((i, tile): itile, output_stack: list(Skel.t)): list(Skel.t) =>
//     switch (Tile.tip(Left, tile), Tile.tip(Right, tile)) {
//     | ((Convex, _), (Convex, _)) => [Op(i), ...output_stack]
//     | ((Convex, _), (Concave, _)) =>
//       switch (output_stack) {
//       | [] => failwith("impossible: pre encountered empty stack")
//       | [skel, ...skels] => [Pre(i, skel), ...skels]
//       }
//     | ((Concave, _), (Convex, _)) =>
//       switch (output_stack) {
//       | [] => failwith("impossible: post encountered empty stack")
//       | [skel, ...skels] => [Post(skel, i), ...skels]
//       }
//     | ((Concave, _), (Concave, _)) =>
//       switch (output_stack) {
//       | []
//       | [_] =>
//         failwith("impossible: bin encountered empty or singleton stack")
//       | [skel1, skel2, ...skels] => [Bin(skel2, i, skel1), ...skels]
//       }
//     };
//   let process_operand = (~output_stack, ~shunted_stack, op) => (
//     output_stack,
//     [op, ...shunted_stack],
//   );
//   let rec process_preop =
//           (
//             ~output_stack: list(Skel.t),
//             ~shunted_stack: list(itile),
//             ipreop: itile,
//           ) => {
//     switch (shunted_stack) {
//     | [] => (output_stack, [ipreop, ...shunted_stack])
//     | [(_, tile) as itile, ...itiles] =>
//       switch (Tile.tip(Right, tile)) {
//       | (Concave, _) => (output_stack, [ipreop, ...shunted_stack])
//       | (Convex, _) =>
//         process_preop(
//           ~output_stack=push_output(itile, output_stack),
//           ~shunted_stack=itiles,
//           ipreop,
//         )
//       }
//     };
//   };
//   // assumes postops lose ties with preops and binops
//   let rec process_postop =
//           (
//             ~output_stack: list(Skel.t),
//             ~shunted_stack: list(itile),
//             (_, post) as ipostop: itile,
//           ) =>
//     switch (shunted_stack) {
//     | [] => (output_stack, [ipostop, ...shunted_stack])
//     | [(_, tile) as itile, ...itiles] =>
//       switch (Tile.tip(Right, tile)) {
//       | (Convex, _) =>
//         process_postop(
//           ~output_stack=push_output(itile, output_stack),
//           ~shunted_stack=itiles,
//           ipostop,
//         )
//       | (Concave, _) =>
//         let p_post = Tile.precedence(post);
//         let p_tile = Tile.precedence(tile);
//         let a_tile = IntMap.find_opt(p_tile, Tile.associativity(tile));
//         p_tile < p_post
//         || p_tile == p_post
//         && a_tile == Some(Associativity.Left)
//           ? process_postop(
//               ~output_stack=push_output(itile, output_stack),
//               ~shunted_stack=itiles,
//               ipostop,
//             )
//           : (output_stack, [ipostop, ...shunted_stack]);
//       }
//     };
//   // currently assumes binops lose ties with preops
//   let rec process_binop =
//           (
//             ~output_stack: list(Skel.t),
//             ~shunted_stack: list(itile),
//             ~precedence as p_bin: Precedent.t,
//             ibinop: itile,
//           ) =>
//     switch (shunted_stack) {
//     | [] => (output_stack, [ibinop, ...shunted_stack])
//     | [(_, tile) as itile, ...itiles] =>
//       switch (tile.mold.shape) {
//       | Op | Post(_) =>
//         process_binop(
//           ~output_stack=push_output(itile, output_stack),
//           ~shunted_stack=itiles,
//           ibinop,
//         )
//       | Pre(p_tile) | Bin(p_tile) =>
//         let a_tile = Associ
//         IntMap.find_opt(p_tile, Tile.associativity(tile));
//         p_tile < p_bin
//         || p_tile == p_bin
//         && a_tile == Some(Associativity.Left)
//           ? process_binop(
//               ~output_stack=push_output(itile, output_stack),
//               ~shunted_stack=itiles,
//               ibinop,
//             )
//           : (output_stack, [ibinop, ...shunted_stack]);
//       }
//     };
//   let rec go =
//           (
//             ~output_stack: list(Skel.t)=[],
//             ~shunted_stack: list(itile)=[],
//             itiles: list(itile),
//           )
//           : list(Skel.t) => {
//     switch (itiles) {
//     | [] =>
//       shunted_stack
//       |> List.fold_left(
//            (output_stack, t) => push_output(t, output_stack),
//            output_stack,
//          )
//     | [(_, tile) as itile, ...itiles] =>
//       let process =
//         switch (tile.mold.shape) {
//         | Op => process_operand
//         | Pre(_) => process_preop
//         | Post(_) => process_postop
//         | Bin(_) => process_binop
//         };
//       let (output_stack, shunted_stack) =
//         process(~output_stack, ~shunted_stack, itile);
//       go(~output_stack, ~shunted_stack, itiles);
//     };
//   };
//   tiles |> List.mapi((i, tile) => (i, tile)) |> go |> List.hd;
// };
