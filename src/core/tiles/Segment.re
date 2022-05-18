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

let shards =
  List.filter_map(
    fun
    | Piece.Shard(s) => Some(s)
    | _ => None,
  );

let contains_matching = (shard: Shard.t) =>
  List.exists(
    fun
    | Piece.Shard(s) => s.tile_id == shard.tile_id
    | _ => false,
  );

let remove_matching = (shard: Shard.t) =>
  List.filter_map(
    fun
    | Piece.Shard(s) when s.tile_id == shard.tile_id => None
    | p => Some(p),
  );

let snoc = (tiles, tile) => tiles @ [tile];

let is_balanced = List.for_all(Piece.is_balanced);

let rec convex = seg => {
  open OptUtil.Syntax;
  let l =
    fold_right(
      (p: Piece.t, shape) => {
        let* s = shape;
        switch (p) {
        | Grout((l, r)) => Nib.Shape.fits(r, s) ? Some(l) : None
        | Shard(sh) =>
          let (l, r) = sh.nibs;
          Nib.Shape.fits(r.shape, s) ? Some(l.shape) : None;
        | Tile(t) =>
          let (l, r) = Mold.nibs(t.mold);
          let convex_kids = List.for_all(convex, t.children);
          convex_kids && Nib.Shape.fits(r.shape, s) ? Some(l.shape) : None;
        };
      },
      seg,
      Some(Nib.Shape.concave()),
    );
  switch (l) {
  | None => false
  | Some(l) => Nib.Shape.fits(Nib.Shape.concave(), l)
  };
};

let split_by_grout = seg =>
  List.fold_right(
    (p: Piece.t, (hd, tl)) =>
      switch (p) {
      | Grout(g) => ([], [(g, hd), ...tl])
      | Tile(_)
      | Shard(_) => ([p, ...hd], tl)
      },
    seg,
    ([], []),
  );

let remold = (seg: t): list(t) =>
  fold_right(
    (p: Piece.t, remolded) => {
      open ListUtil.Syntax;
      let+ seg = remolded
      and+ p = Piece.remold(p);
      [p, ...seg];
    },
    seg,
    [empty],
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

let rec shape = (seg: t, r: Nib.Shape.t) =>
  switch (seg) {
  | [] => r
  | [Grout(_), ...seg] => shape(seg, r)
  | [Shard(s), ..._] =>
    let (l, _) = s.nibs;
    l.shape;
  | [Tile(t), ..._] =>
    let (l, _) = Mold.nibs(t.mold);
    l.shape;
  };

let rec regrout = ((l, r): (Nib.Shape.t, Nib.Shape.t), seg: t) => {
  let regrouted =
    fold_right(
      (p: Piece.t, regrouted: t) =>
        switch (p) {
        | Grout((s_l, s_r)) =>
          switch (regrouted) {
          | [Grout((_, s_r')), ...regrouted] => [
              Grout((s_l, s_r')),
              ...regrouted,
            ]
          | _ =>
            if (Nib.Shape.fits(s_r, shape(regrouted, r))) {
              [p, ...regrouted];
            } else if (Nib.Shape.fits(s_l, s_r)) {
              [Grout((l, Nib.Shape.flip(r))), ...regrouted];
            } else {
              regrouted;
            }
          }
        | Shard(shard) =>
          let (_, n_r) = shard.nibs;
          let s_r = n_r.shape;
          switch (regrouted) {
          | [Grout((s_l', s_r')) as g, ...tl] =>
            if (Nib.Shape.fits(s_r, s_l')) {
              [p, g, ...tl];
            } else if (Nib.Shape.fits(s_l', s_r')) {
              [p, Grout((Nib.Shape.flip(s_l'), s_r')), ...tl];
            } else {
              [p, ...tl];
            }
          | _ =>
            Nib.Shape.fits(s_r, shape(regrouted, r))
              ? [p, ...regrouted]
              : [p, Grout(Grout.mk_fits_shape(s_r)), ...regrouted]
          };
        | Tile(tile) =>
          let p =
            Piece.Tile({
              ...tile,
              children:
                List.map(
                  regrout((Nib.Shape.concave(), Nib.Shape.concave())),
                  tile.children,
                ),
            });
          let (_, n_r) = Mold.nibs(tile.mold);
          let s_r = n_r.shape;
          switch (regrouted) {
          | [Grout((s_l', s_r')) as g, ...tl] =>
            if (Nib.Shape.fits(s_r, s_l')) {
              [p, g, ...tl];
            } else if (Nib.Shape.fits(s_l', s_r')) {
              [p, Grout((Nib.Shape.flip(s_l'), s_r')), ...tl];
            } else {
              [p, ...tl];
            }
          | _ =>
            Nib.Shape.fits(s_r, shape(regrouted, r))
              ? [p, ...regrouted]
              : [p, Grout(Grout.mk_fits_shape(s_r)), ...regrouted]
          };
        },
      seg,
      empty,
    );
  switch (regrouted) {
  | [Grout(g), ...tl] => Grout.fits_shape(g, l) ? regrouted : tl
  | _ =>
    Nib.Shape.fits(l, shape(regrouted, r))
      ? regrouted : [Grout(Grout.mk_fits_shape(l)), ...regrouted]
  };
};

module Stack = Stack.Make(Orientation.R);
// TODO use direction parameter
let reassemble = (seg: t): t =>
  List.fold_right(Stack.push, seg, Stack.init) |> Stack.flatten;
