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
          let s' = tile.mold.out;
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

// let mk_grout = (l, r) =>
//   Grout.mk_fits((l, r)) |> Option.map(Piece.grout) |> Option.to_list;

module Trim = {
  type seg = t;
  type t = Aba.t(list(Whitespace.t), Grout.t);

  let empty = Aba.mk([[]], []);

  let cons_w = (w: Whitespace.t, (wss, gs)) => {
    // safe bc Aba always has at least one A element
    let (ws, wss) = ListUtil.split_first(wss);
    Aba.mk([[w, ...ws], ...wss], gs);
  };
  let cons_g = (g: Grout.t, (wss, gs)) =>
    Aba.mk([[], ...wss], [g, ...gs]);

  let ws = ((wss, _): t): seg => List.(map(Piece.whitespace, concat(wss)));

  // postcond: result is either <ws> or <ws,g,ws'>
  let merge = ((wss, gs): t): t => {
    switch (Grout.merge(gs)) {
    | None => Aba.mk([List.concat(wss)], [])
    | Some(g) =>
      let (ws, wss) = ListUtil.split_first(wss);
      Aba.mk([ws, List.concat(wss)], [g]);
    };
  };
  // same as merge but type encodes postcond
  // let merged = (trim: t): (list(Whitespace.t), option((Grout.t, list(Whitespace.t)))) => {
  //   let (wss, gs) = merge(trim);
  //   let (ws, wss) = ListUtil.split_first(wss);
  //   switch (gs) {
  //   | [] => (ws, None)
  //   | [g, ..._] => (ws, Some((g, List.concat(wss))))
  //   };
  // };

  // assumes grout in trim fit r but may not fit l
  let regrout = ((l, r): (Nib.Shape.t, Nib.Shape.t), trim: t): t =>
    if (Nib.Shape.fits(l, r)) {
      let (wss, _) = trim;
      Aba.mk([List.concat(wss)], []);
    } else {
      let (_, gs) as merged = merge(trim);
      switch (gs) {
      | [] => cons_g(Grout.mk_fits_shape(l), merged)
      | [_, ..._] => merged
      };
    };

  let to_seg = (trim: t) =>
    trim
    |> Aba.join(List.map(Piece.whitespace), g => [Piece.Grout(g)])
    |> List.concat;
};

let rec regrout = ((l, r), seg) => {
  let (trim, r, tl) = regrout_tl(seg, r);
  let trim = Trim.regrout((l, r), trim);
  Trim.to_seg(trim) @ tl;
}
and regrout_tl = (seg: t, r: Nib.Shape.t): (Trim.t, Nib.Shape.t, t) =>
  fold_right(
    (p: Piece.t, (trim, r, tl)) =>
      switch (p) {
      | Whitespace(w) => (Trim.cons_w(w, trim), r, tl)
      | Grout(g) => (Trim.(merge(cons_g(g, trim))), r, tl)
      | Shard(s) =>
        let (l', r') = TupleUtil.map2(Nib.shape, s.nibs);
        let trim = trim |> Trim.regrout((r', r)) |> Trim.to_seg;
        (Trim.empty, l', [p, ...trim] @ tl);
      | Tile(t) =>
        let children =
          List.map(regrout(Nib.Shape.(concave(), concave())), t.children);
        let p = Piece.Tile({...t, children});
        let (l', r') = TupleUtil.map2(Nib.shape, Mold.nibs(t.mold));
        let trim = trim |> Trim.regrout((r', r)) |> Trim.to_seg;
        (Trim.empty, l', [p, ...trim] @ tl);
      },
    seg,
    (Aba.mk([[]], []), r, empty),
  );

// for internal use when dealing with segments in reverse order (eg Affix.re)
let flip_nibs =
  List.map(
    fun
    | (Piece.Whitespace(_) | Grout(_)) as p => p
    | Shard(s) => Shard({...s, nibs: Nibs.flip(s.nibs)})
    | Tile(t) => Tile({...t, mold: Mold.flip_nibs(t.mold)}),
  );

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
