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

let rec shape =
        (seg: t, r: Nib.Shape.t)
        : (Aba.t(list(Whitespace.t), Grout.t), Nib.Shape.t, t) => {
  let empty_wgw = Aba.mk([[]], []);
  switch (seg) {
  | [] => (empty_wgw, r, [])
  | [p, ...tl] =>
    let (wgw, s, tl) = shape(tl, r);
    switch (p) {
    | Whitespace(w) =>
      let (wss, gs) = wgw;
      let (ws, wss) = ListUtil.split_first(wss);
      (([[w, ...ws], ...wss], gs), s, tl);
    | Grout(g) => (Aba.cons([], g, wgw), s, tl)
    | Shard(s) =>
      let (l, _) = s.nibs;
      (empty_wgw, l.shape, tl);
    | Tile(t) =>
      let (l, _) = Mold.nibs(t.mold);
      (empty_wgw, l.shape, tl);
    };
  };
};

let rec convex = seg => {
  open OptUtil.Syntax;
  let l =
    fold_right(
      (p: Piece.t, shape) => {
        let* s = shape;
        switch (p) {
        | Whitespace(_) => shape
        | Grout(g) =>
          Grout.fits_shape(g, s) ? Some(fst(Grout.shapes(g))) : None
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

let split_by_grout: t => Aba.t(t, Grout.t) =
  Aba.split(
    fun
    | Piece.Grout(g) => Either.R(g)
    | p => L(p),
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
        | Whitespace(_)
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
        | Whitespace(_)
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

// let rec shape = (seg: t, r: Nib.Shape.t) =>
//   switch (seg) {
//   | [] => r
//   | [Grout(_), ...seg] => shape(seg, r)
//   | [Shard(s), ..._] =>
//     let (l, _) = s.nibs;
//     l.shape;
//   | [Tile(t), ..._] =>
//     let (l, _) = Mold.nibs(t.mold);
//     l.shape;
//   };

let rec regrout = ((l, r): (Nib.Shape.t, Nib.Shape.t), seg: t) => {
  let mk_fits = (l, r) => Option.to_list(Grout.mk_fits((l, r)));
  switch (seg) {
  | [] => mk_fits(l, r)
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
      let (l', r') = TupleUtil.map2(Nib.shape, Mold.nibs(t.mold));
      mk_fits(l, l') @ [hd, ...regrout((r', r), tl)];
    | Shard(shard) =>
      let (l', r') = TupleUtil.map2(Nib.shape, shard.nibs);
      mk_fits(l, l') @ [hd, ...regrout((l, r), tl)];
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
    }
  };
};

let split_by_matching_shard = (tile_id: Id.t): (t => Aba.t(t, Shard.t)) =>
  Aba.split(
    fun
    | Piece.Shard(s) when s.tile_id == tile_id => Either.R(s)
    | p => L(p),
  );

module Match = Tile.Match.Make(Orientation.R);
let rec reassemble = (seg: t): t =>
  switch (shards(seg)) {
  | [] => seg
  | [s, ..._] =>
    switch (Aba.trim(split_by_matching_shard(s.tile_id, seg))) {
    | None => seg
    | Some((seg_l, match, seg_r)) =>
      let seg_m =
        switch (Match.complete(match)) {
        | None => Match.join(match)
        | Some(t) =>
          let children = List.map(reassemble, t.children);
          [Tile.to_piece({...t, children})];
        };
      List.concat([seg_l, seg_m, reassemble(seg_r)]);
    }
  };
