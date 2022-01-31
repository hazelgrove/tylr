// open Util;
// open OptUtil.Syntax;

let disassemble_tile = (tile: Tile.t): Segment.t =>
  tile.tokens
  |> Util.Aba.mapi_a((index, _) =>
       Shard.{
         tile_id: tile.id,
         form: (index, Tile.form(tile)),
         nibs: Tile.nibs(~index, tile.mold),
       }
     )
  |> Util.Baba.cons(Tiles.empty)
  |> Fun.flip(Util.Aba.snoc, Tiles.empty);

let reassemble_segment = _ => failwith("todo Parser.assemble_segment");

let disassemble_ancestor = _ => failwith("todo Parser.disassemble_ancestor");
let reassemble_relatives = (_, _) =>
  failwith("todo Parser.assemble_relatives");

// let assemble_affix = (_, _) => failwith("todo Parser.assemble_affix");
// let assemble_affixes = _ => failwith("todo Parser.assemble_affixes");

// let split_by_matching_shards =
//     ((id, shard): Shard.t, segment: Segment.t)
//     : (AltList.t(Shard.t, Segment.t), Segment.t) => {
//   let (tl, rest) =
//     AltList.split(
//       fun
//       | Piece.Shard((id', _) as shard) when id == id' =>
//         Some(shard)
//       | _ => None,
//       segment,
//     );
//   ((shard, tl), rest);
// };

// let split_by_matching_shards =
//     ((_nibs, (id, _token)) as hd: Nibbed.t(Shard.t), segment: Segment.t)
//     : (Aba.t(Nibbed.t(Shard.t), Segment.t), Segment.t) => {
//   let (tl, rest) =
//     segment
//     |> Aba.split(
//       fun
//       | ((id', _) as shard) when id' == id => Some(shard)
//       | _ => None
//     )
//     |> Aba.split_last;
//   ((hd, tl), rest);
// };

// let rec assemble_segment = (d: Direction.t, segment: Segment.t): Segment.t =>
//   switch (segment) {
//   | (_, []) => segment
//   | (tiles, [(shard, tiles'), ...segment]) =>
//     let (split, rest) = split_by_matching_shards(shard, (tiles', segment));
//     assemble_tile(d, split) @ assemble_segment(d, rest)
//   }
// // attempt to assemble tile from specified shards
// // + assemble inner segments
// and assemble_tile =
//     (d: Direction.t, split: Aba.t(Shard.t, Segment.t))
//     : Segment.t => {
//   let id = Shard.id(Aba.hd(split));
//   let of_tile = tile => Segment.of_tiles([(id, tile)]);
//   let split =
//     split
//     |> Aba.map_a(Identified.get(Fun.id))
//     // using hd relies on well-balanced invariant
//     // to guarantee pieces aren't being dropped
//     |> Aba.map_b(segment => Aba.hd(assemble_segment(segment)));
//   let ordered_split =
//     switch (d) {
//     | Left => Aba.rev(Aba.map_b(List.rev, split))
//     | Right => split
//     };
//   switch (ordered_split) {
//   | ("(", [(body, ")")]) => of_tile(Paren(body))
//   | ("[", [(arg, "]")]) => of_tile(Ap(arg))
//   | ("λ", [(p, "{"), (body, "}")]) => of_tile(Lam(p, body))
//   | ("let", [(p, "="), (def, "in")]) => of_tile(Let(p, def))
//   | ("?", [(then_, ":")]) => of_tile(Cond(then_))
//   | _ =>
//     split
//     |> Aba.map_a(shard => (id, shard))
//     |> Baba.cons([])
//     |> Aba.snoc([]);
//   };
// };

// let disassemble_frame = (frames: Frame.t) =>
//   switch (tile_frames) {
//   | [] => None
//   | [((id, (tile_pre, tile_suf)), (prefix, suffix)), ...tile_frames] =>

//   }

//  {
//   let+ ((tile_pre, tile_suf), ((prefix, suffix), frame)) = frame;
//   let prefix = disassemble_a(id, tile_pre) @ Segment.of_tiles(prefix);
//   let suffix = disassemble_a(id, tile_suf) @ Segment.of_tiles(suffix);
//   ((prefix, suffix), frame);
// };

// // assumes shards have matching ids
// let assemble_frame =
//     (
//       frame_t: (AltList.t(Shard.t, Tiles.t) as 't, 't),
//       (prefix, suffix): Segment.frame,
//       frame: Frame.t,
//     )
//     : option(Frame.t) => {
//   let* prefix = Segment.get_tiles(prefix);
//   let* suffix = Segment.get_tiles(suffix);
//   switch (frame_t, frame) {
//   | (((Pat((id, Paren_l)), []), (Pat((_, Paren_r)), [])), Pat(frame)) =>
//     let+ prefix = Tiles.get_pat(prefix)
//     and+ suffix = Tiles.get_pat(suffix);
//     Frame.Pat((id, Paren_body(((prefix, suffix), frame))));
//   | (
//       (
//         (Exp((id, Lam_lam)), []),
//         (Exp((_, Lam_open)), [(body, Exp((_, Lam_close)))]),
//       ),
//       Exp(frame),
//     ) =>
//     let+ body = Tiles.get_exp(body)
//     and+ prefix = Tiles.get_exp(prefix)
//     and+ suffix = Tiles.get_exp(suffix);
//     Frame.Pat((id, Lam_pat(body, ((prefix, suffix), frame))));
//   | (
//       (
//         (Exp((id, Let_let)), []),
//         (Exp((_, Let_eq)), [(def, Exp((_, Let_in)))]),
//       ),
//       Exp(frame),
//     ) =>
//     let+ def = Tiles.get_exp(def)
//     and+ prefix = Tiles.get_exp(prefix)
//     and+ suffix = Tiles.get_exp(suffix);
//     Frame.Pat((id, Let_pat(def, ((prefix, suffix), frame))));
//   | (((Exp((id, Paren_l)), []), (Exp((_, Paren_r)), [])), Exp(frame)) =>
//     let+ prefix = Tiles.get_exp(prefix)
//     and+ suffix = Tiles.get_exp(suffix);
//     Frame.Exp((id, Paren_body(((prefix, suffix), frame))));
//   | (((Exp((id, Ap_l)), []), (Exp((_, Ap_r)), [])), Exp(frame)) =>
//     let+ prefix = Tiles.get_exp(prefix)
//     and+ suffix = Tiles.get_exp(suffix);
//     Frame.Exp((id, Ap_arg(((prefix, suffix), frame))));
//   | (
//       ((Exp((id, Lam_lam_open(p))), []), (Exp((_, Lam_close)), [])),
//       Exp(frame),
//     ) =>
//     let+ prefix = Tiles.get_exp(prefix)
//     and+ suffix = Tiles.get_exp(suffix);
//     Frame.Exp((id, Lam_body(p, ((prefix, suffix), frame))));
//   | (
//       ((Exp((id, Let_let_eq(p))), []), (Exp((_, Let_in)), [])),
//       Exp(frame),
//     ) =>
//     let+ prefix = Tiles.get_exp(prefix)
//     and+ suffix = Tiles.get_exp(suffix);
//     Frame.Exp((id, Let_def(p, ((prefix, suffix), frame))));
//   | (((Exp((id, Cond_que)), []), (Exp((_, Cond_col)), [])), Exp(frame)) =>
//     let+ prefix = Tiles.get_exp(prefix)
//     and+ suffix = Tiles.get_exp(suffix);
//     Frame.Exp((id, Cond_then(((prefix, suffix), frame))));
//   | _ => None
//   };
// };

// // assumes shards have matching ids
// let assemble_frame =
//     (
//       disassembled: (AltList.t(Shard.t, Segment.t) as 't, 't),
//       (prefix, suffix): Segment.frame,
//       frame: Frame.t,
//     )
//     : option(Frame.t) => {
//   let* prefix = Segment.get_tiles(prefix);
//   let* suffix = Segment.get_tiles(suffix);
//   switch (disassembled, frame) {
//   | (((Pat((id, Paren_l)), []), (Pat((_, Paren_r)), [])), Pat(frame)) =>
//     let+ prefix = Tiles.get_pat(prefix)
//     and+ suffix = Tiles.get_pat(suffix);
//     Frame.Pat((id, Paren_body(((prefix, suffix), frame))));
//   | (
//       (
//         (Exp((id, Lam_lam)), []),
//         (Exp((_, Lam_open)), [(body, Exp((_, Lam_close)))]),
//       ),
//       Exp(frame),
//     ) =>
//     let+ body = Segment.get_tiles_exp(body)
//     and+ prefix = Tiles.get_exp(prefix)
//     and+ suffix = Tiles.get_exp(suffix);
//     Frame.Pat((id, Lam_pat(body, ((prefix, suffix), frame))));
//   | (
//       (
//         (Exp((id, Let_let)), []),
//         (Exp((_, Let_eq)), [(def, Exp((_, Let_in)))]),
//       ),
//       Exp(frame),
//     ) =>
//     let+ def = Segment.get_tiles_exp(def)
//     and+ prefix = Tiles.get_exp(prefix)
//     and+ suffix = Tiles.get_exp(suffix);
//     Frame.Pat((id, Let_pat(def, ((prefix, suffix), frame))));
//   | (((Exp((id, Paren_l)), []), (Exp((_, Paren_r)), [])), Exp(frame)) =>
//     let+ prefix = Tiles.get_exp(prefix)
//     and+ suffix = Tiles.get_exp(suffix);
//     Frame.Exp((id, Paren_body(((prefix, suffix), frame))));
//   | (((Exp((id, Ap_l)), []), (Exp((_, Ap_r)), [])), Exp(frame)) =>
//     let+ prefix = Tiles.get_exp(prefix)
//     and+ suffix = Tiles.get_exp(suffix);
//     Frame.Exp((id, Ap_arg(((prefix, suffix), frame))));
//   | (
//       ((Exp((id, Lam_lam_open(p))), []), (Exp((_, Lam_close)), [])),
//       Exp(frame),
//     ) =>
//     let+ prefix = Tiles.get_exp(prefix)
//     and+ suffix = Tiles.get_exp(suffix);
//     Frame.Exp((id, Lam_body(p, ((prefix, suffix), frame))));
//   | (
//       ((Exp((id, Let_let_eq(p))), []), (Exp((_, Let_in)), [])),
//       Exp(frame),
//     ) =>
//     let+ prefix = Tiles.get_exp(prefix)
//     and+ suffix = Tiles.get_exp(suffix);
//     Frame.Exp((id, Let_def(p, ((prefix, suffix), frame))));
//   | (((Exp((id, Cond_que)), []), (Exp((_, Cond_col)), [])), Exp(frame)) =>
//     let+ prefix = Tiles.get_exp(prefix)
//     and+ suffix = Tiles.get_exp(suffix);
//     Frame.Exp((id, Cond_then(((prefix, suffix), frame))));
//   | _ => None
//   };
// };

// let rec assemble_zipper =
//     (affixes: Segment.frame, frame: Frame.t)
//     : (Segment.frame, Frame.t) => {
//   switch (affixes) {
//   | ([], _)
//   | (_, []) => (affixes, frame)
//   | ([Tile(_) as tile, ...prefix], suffix) =>
//     let ((prefix, suffix), frame) =
//       assemble_zipper((prefix, suffix), frame);
//     (([tile, ...prefix], suffix), frame);
//   | (prefix, [Tile(_) as tile, ...suffix]) =>
//     let ((prefix, suffix), frame) =
//       assemble_zipper((prefix, suffix), frame);
//     ((prefix, [tile, ...suffix]), frame);
//   | ([Shard(shard_pre), ...prefix], [Shard(shard_suf), ...suffix]) =>
//     let (disassembled_pre, prefix) = split_by_matching_shards(shard_pre, prefix);
//     let (disassembled_suf, suffix) = split_by_matching_shards(shard_suf, suffix);
//     let ((prefix, suffix), frame) = assemble_zipper((prefix, suffix), frame);
//     switch (
//       assemble_frame((disassembled_pre, disassembled_suf), (prefix, suffix), frame)
//     ) {
//     | None =>
//       let prefix = flatten(disassembled_pre) @ prefix;
//       let suffix = flatten(disassembled_suf) @ suffix;
//       ((prefix, suffix), frame);
//     | Some(frame) => (([], []), frame)
//     }
//   }
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
