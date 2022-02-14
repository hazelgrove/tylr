open Util;

[@deriving sexp]
type t = Tile.s;

// type hd = Grouts.t;
// type tl = Util.Baba.t(Tile.t, Grouts.t);

// let cons_grout = (grout, (hd, tl)) => ([grout, ...hd], tl);
// let cons_tile = (tile, (hd, tl)) => ([], [(tile, hd), ...tl]);

let empty = [];
let rev = _ => failwith("todo Tiles.rev");
let concat = _ => failwith("todo Tiles.concat");

let of_shard = _ => failwith("todo of_shard");

let shards = _ => failwith("todo shards");

let cons = List.cons;
let snoc = (tiles, tile) => tiles @ [tile];

let is_intact = List.for_all(Tile.is_intact);

let remove_matching = (_, _) => failwith("todo remove_matching");

// let mk: list(Tile.t) => t = _ => failwith("todo Tiles.mk");

let split_by_grout = (_tiles: t): Util.Aba.t(t, Grout.t) =>
  failwith("todo split_by_grout");

let rec split_by_matching_shard = (tiles: t): Util.Aba.t(t, Shard.t) => {
  let (split, _) =
    List.fold_right(
      (tile: Tile.t, (split_suffix, found_shard)) =>
        switch (tile) {
        | Intact(_) =>
          let (hd, tl) = split_suffix;
          (([tile, ...hd], tl), found_shard);
        | Pieces(pieces) =>
          let (split_p, found) =
            split_pieces_by_matching_shard(pieces, found_shard);
          (Aba.concat((@), split_p, split_suffix), found);
        },
      tiles,
      (([], []), None),
    );
  split;
}
and split_pieces_by_matching_shard =
    (pieces: Tile.pieces, found_shard: option(Id.t))
    : ((Util.Aba.t(t, Shard.t), option(Id.t)) as 'r) => {
  let cons_piece = (piece: Piece.t, ((hd, tl), found)): 'r =>
    switch (piece, found) {
    | (Shard({tile: (id, _), _} as labeled), None) => (
        ([], [(labeled, hd), ...tl]),
        Some(id),
      )
    | (Shard({tile: (id, _), _} as labeled), Some(id')) when id == id' => (
        ([], [(labeled, hd), ...tl]),
        found,
      )
    | (piece, _) => (([Tile.Pieces((piece, [])), ...hd], tl), found)
    };
  pieces
  |> Aba.fold_right(
       piece => cons_piece(piece, (([], []), found_shard)),
       (piece, tiles, (split_suffix, found)) => {
         let split_tiles = split_by_matching_shard(tiles);
         cons_piece(
           piece,
           (Aba.concat((@), split_tiles, split_suffix), found),
         );
       },
     );
};

let rec reassemble = (tiles: t): t => {
  let (hd, tl) = split_by_matching_shard(tiles);
  switch (Baba.split_last(tl)) {
  | None => hd
  | Some((tile_split, last)) =>
    let tile = Tile.mk(~reassemble, tile_split);
    reassemble(hd) @ [tile, ...last];
  };
};

/**
 * should never need
 */;
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

// let connect = (~insert=empty, affixes: Frame.t, s: Sort.t): (t, Frame.t) => {
//   let ctx = failwith("todo ctx");
//   let ((tiles_l, prefix_tl), (tiles_r, suffix_tl)) = affixes;
//   let nibs = Frame.near_nibs_tls((prefix_tl, suffix_tl), Nibs.of_sort(s));
//   let (insertion, grouts, tiles) =
//     remold_reshape(insert, (tiles_l, tiles_r), nibs, ctx)
//     |> List.sort(((_, grouts, _), (_, grouts', _)) =>
//          Grouts.Frame.(Int.compare(size(grouts), size(grouts')))
//        )
//     |> ListUtil.hd_opt
//     |> OptUtil.get_or_fail("Segment.connect: expected at least one remolding");
//   (insertion, Frame.(concat([of_grouts(grouts), of_tiles(tiles)])));
// };
let connect = (~insert as _=empty, _siblings, _s) =>
  failwith("todo connect");
