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

let remold = (seg: t): list(t) =>
  fold_right(
    (p: Piece.t, remolded) => {
      open ListUtil.Syntax;
      let+ seg = remolded
      and+ p = Piece.remold(p);
      [p, ...seg];
    },
    seg,
    [],
  );

let rec sort_rank = (seg: t, (s_l, s_r): (Sort.t, Sort.t)) => {
  let (s_l', rank) =
    fold_right(
      (p: Piece.t, (s, rank)) =>
        switch (p) {
        | Grout(_) => (s, rank)
        | Shard(shard) =>
          let (n_l, n_r) = shard.nibs;
          let (s_l, s_r) = (n_l.sort, n_r.sort);
          (s_l, rank + Bool.to_int(s_r != s));
        | Tile(tile) =>
          let s' = tile.mold.sorts.out;
          let children_ranks =
            Tile.sorted_children(tile)
            |> List.map(((s, child)) => sort_rank(child, (s, s)))
            |> List.fold_left((+), 0);
          let rank' = rank + children_ranks + Bool.to_int(s != s');
          (s', rank');
        },
      seg,
      (s_r, 0),
    );
  rank + Bool.to_int(!Sort.consistent(s_l, s_l'));
};

let rec shape_rank = (seg, (s_l, s_r): (Nib.Shape.t, Nib.Shape.t)) => {
  let (s_l', rank) =
    fold_right(
      (p: Piece.t, (s, rank)) =>
        switch (p) {
        | Grout(_) => (s, rank)
        | Shard(shard) =>
          let (n_far, n_near) = shard.nibs;
          let (s_far, s_near) = (n_far.shape, n_near.shape);
          (s_near, rank + Bool.to_int(!Nib.Shape.fits(s_far, s)));
        | Tile(tile) =>
          let (n_far, n_near) = Mold.nibs(tile.mold);
          let (s_far, s_near) = (n_far.shape, n_near.shape);
          let children_ranks =
            tile.children
            |> List.map(child =>
                 shape_rank(child, Nib.Shape.(concave(), concave()))
               )
            |> List.fold_left((+), 0);
          let rank' =
            rank + children_ranks + Bool.to_int(!Nib.Shape.fits(s_far, s));
          (s_near, rank');
        },
      seg,
      (s_r, 0),
    );
  rank + Bool.to_int(!Nib.Shape.fits(s_l, s_l'));
};

module Stack = Stack.Make(Orientation.R);
// TODO use direction parameter
let reassemble = (seg: t): t =>
  List.fold_right(Stack.push, seg, Stack.init) |> Stack.flatten;