open Util;

exception Empty_segment;

[@deriving show]
type t = Base.segment;

let empty = [];
let cons = List.cons;
let concat = List.concat;
let fold_right = List.fold_right;
let rev = List.rev;

let of_tile = t => [Tile.to_piece(t)];

let nibs = tiles =>
  switch (tiles, ListUtil.split_last_opt(tiles)) {
  | ([], _)
  | (_, None) => None
  | ([_first, ..._], Some((_, _last))) => failwith("todo Tiles.nibs")
  };

let incomplete_tiles =
  List.filter_map(
    fun
    | Piece.Tile(t) when !Tile.is_complete(t) => Some(t)
    | _ => None,
  );

let contains_matching = (t: Tile.t) =>
  List.exists(
    fun
    | Piece.Tile(t') => t'.id == t.id
    | _ => false,
  );

let remove_matching = (t: Tile.t) =>
  List.filter_map(
    fun
    | Piece.Tile(t') when t'.id == t.id => None
    | p => Some(p),
  );

let snoc = (tiles, tile) => tiles @ [tile];

// let is_balanced = List.for_all(Piece.is_balanced);

let shape_affix =
    (d: Direction.t, affix: t, r: Nib.Shape.t)
    : (Aba.t(list(Whitespace.t), Grout.t), Nib.Shape.t, t) => {
  let empty_wgw = Aba.mk([[]], []);
  let rec go = (affix: t, r: Nib.Shape.t) =>
    switch (affix) {
    | [] => (empty_wgw, r, [])
    | [p, ...tl] =>
      let (wgw, s, tl) = go(tl, r);
      switch (p) {
      | Whitespace(w) =>
        let (wss, gs) = wgw;
        let (ws, wss) = ListUtil.split_first(wss);
        (([[w, ...ws], ...wss], gs), s, tl);
      | Grout(g) => (Aba.cons([], g, wgw), s, tl)
      | Tile(t) =>
        let (l, _) = Tile.shapes(t) |> (d == Left ? TupleUtil.swap : Fun.id);
        (empty_wgw, l, tl);
      };
    };
  go((d == Left ? List.rev : Fun.id)(affix), r);
};
let shape = shape_affix(Right);

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
        | Tile(t) =>
          let (l, r) = Tile.shapes(t);
          List.for_all(convex, t.children) && Nib.Shape.fits(r, s)
            ? Some(l) : None;
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

let skel = seg =>
  seg
  |> List.mapi((i, p) => (i, p))
  |> List.filter_map(((i, p)) =>
       Piece.shapes(p) |> Option.map(ss => (i, ss))
     )
  |> Skel.mk;

let sorted_children = seg =>
  seg |> List.map(Piece.sorted_children) |> List.concat;

let sort_rank_root = (seg: t, s: Sort.t) => {
  let mold: Piece.t => Mold.t =
    fun
    | Whitespace(_) => failwith("impossible since skel ignores whitespace")
    | Grout(g) =>
      // TODO(d) refactor to always return Any
      Mold.of_grout(g, Any)
    | Tile(t) => t.mold;

  let rec go = (sort, skel: Skel.t) => {
    let root_mold = mold(List.nth(seg, Skel.root_index(skel)));
    let r = !Sort.consistent(sort, root_mold.out) |> Bool.to_int;
    // let go_s = sorts_skels =>
    //   sorts_skels
    //   |> List.map(((sort, skel)) => go(sort, skel))
    //   |> List.fold_left((+)(0));
    let unichild_rs =
      switch (skel) {
      | Op(_) => 0
      | Pre(_, r) => go(snd(root_mold.nibs).sort, r)
      | Post(l, _) => go(fst(root_mold.nibs).sort, l)
      | Bin(l, _, r) =>
        go(fst(root_mold.nibs).sort, l) + go(snd(root_mold.nibs).sort, r)
      };
    r + unichild_rs;
  };

  go(s, skel(seg));
};

let rec sort_rank = (seg: t, s: Sort.t) =>
  sort_rank_root(seg, s) + sort_rank_desc(seg)
and sort_rank_desc = (seg: t) =>
  sorted_children(seg)
  |> List.map(((s, seg)) => sort_rank(seg, s))
  |> List.fold_left((+), 0);

let rec shape_rank = (seg, (l, r): (Nib.Shape.t, Nib.Shape.t)) => {
  let (l', rank) = shape_rank_affix(Direction.Right, seg, r);
  rank + Bool.to_int(!Nib.Shape.fits(l, l'));
}
and shape_rank_affix = (d: Direction.t, seg, r: Nib.Shape.t) =>
  fold_right(
    (p: Piece.t, (r, rank)) =>
      switch (p) {
      | Whitespace(_)
      | Grout(_) => (r, rank)
      | Tile(t) =>
        let (l, r') =
          Tile.shapes(t) |> (d == Left ? TupleUtil.swap : Fun.id);
        let children_ranks =
          t.children
          |> List.map(child =>
               shape_rank(child, Nib.Shape.(concave(), concave()))
             )
          |> List.fold_left((+), 0);
        let rank' =
          rank + children_ranks + Bool.to_int(!Nib.Shape.fits(r', r));
        (l, rank');
      },
    (d == Left ? List.rev : Fun.id)(seg),
    (r, 0),
  );

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

  let rev = Aba.rev(List.rev, Fun.id);

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
  let regrout = ((l, r): (Nib.Shape.t, Nib.Shape.t), trim: t): IdGen.t(t) =>
    if (Nib.Shape.fits(l, r)) {
      let (wss, _) = trim;
      IdGen.return(Aba.mk([List.concat(wss)], []));
    } else {
      let (_, gs) as merged = merge(trim);
      switch (gs) {
      | [] =>
        open IdGen.Syntax;
        let+ g = Grout.mk_fits_shape(l);
        cons_g(g, merged);
      | [_, ..._] => IdGen.return(merged)
      };
    };

  let to_seg = (trim: t) =>
    trim
    |> Aba.join(List.map(Piece.whitespace), g => [Piece.Grout(g)])
    |> List.concat;
};

let rec regrout = ((l, r), seg) => {
  open IdGen.Syntax;
  let* (trim, r, tl) = regrout_affix(Direction.Right, seg, r);
  let+ trim = Trim.regrout((l, r), trim);
  Trim.to_seg(trim) @ tl;
}
and regrout_affix =
    (d: Direction.t, affix: t, r: Nib.Shape.t)
    : IdGen.t((Trim.t, Nib.Shape.t, t)) => {
  open IdGen.Syntax;
  let+ (trim, s, affix) =
    fold_right(
      (p: Piece.t, id_gen) => {
        let* (trim, r, tl) = id_gen;
        switch (p) {
        | Whitespace(w) => IdGen.return((Trim.cons_w(w, trim), r, tl))
        | Grout(g) => IdGen.return((Trim.(merge(cons_g(g, trim))), r, tl))
        | Tile(t) =>
          let* children =
            List.fold_right(
              (hd, tl) => {
                let* tl = tl;
                let+ hd = regrout(Nib.Shape.(concave(), concave()), hd);
                [hd, ...tl];
              },
              t.children,
              IdGen.return([]),
            );
          let p = Piece.Tile({...t, children});
          let (l', r') =
            Tile.shapes(t) |> (d == Left ? TupleUtil.swap : Fun.id);
          let+ trim = Trim.regrout((r', r), trim);
          (Trim.empty, l', [p, ...Trim.to_seg(trim)] @ tl);
        };
      },
      (d == Left ? List.rev : Fun.id)(affix),
      IdGen.return((Aba.mk([[]], []), r, empty)),
    );
  d == Left ? (Trim.rev(trim), s, rev(affix)) : (trim, s, affix);
};

// for internal use when dealing with segments in reverse order (eg Affix.re)
// let flip_nibs =
//   List.map(
//     fun
//     | (Piece.Whitespace(_) | Grout(_)) as p => p
//     | Tile(t) => Tile({...t, mold: Mold.flip_nibs(t.mold)}),
//   );

let split_by_matching = (id: Id.t): (t => Aba.t(t, Tile.t)) =>
  Aba.split(
    fun
    | Piece.Tile(t) when t.id == id => Either.R(t)
    | p => L(p),
  );

// module Match = Tile.Match.Make(Orientation.R);
let rec reassemble = (seg: t): t =>
  switch (incomplete_tiles(seg)) {
  | [] => seg
  | [t, ..._] =>
    switch (Aba.trim(split_by_matching(t.id, seg))) {
    | None => seg
    | Some((seg_l, match, seg_r)) =>
      let t = Tile.reassemble(match);
      let children = List.map(reassemble, t.children);
      let p = Tile.to_piece({...t, children});
      seg_l @ [p, ...reassemble(seg_r)];
    }
  };

let trim_whitespace: (Direction.t, t) => t =
  (d, ps) => {
    /* Trims leading/trailing whitespace */
    let rec trim_l = xs =>
      switch (xs) {
      | [] => []
      | [Piece.Whitespace(_), ...xs] => trim_l(xs)
      | [_, ..._] => xs
      };
    switch (d) {
    | Left => ps |> trim_l
    | Right => ps |> List.rev |> trim_l |> List.rev
    };
  };
let trim_whitespace_and_grout: (Direction.t, t) => t =
  (d, ps) => {
    /* Trims leading/trailing whitespace, continuing
       to trim around grout until first Tile is reached */
    let rec trim_l = xs =>
      switch (xs) {
      | [] => []
      | [Piece.Whitespace(_) | Piece.Grout(_), ...xs] => trim_l(xs)
      | [_, ..._] => xs
      };
    switch (d) {
    | Left => ps |> trim_l
    | Right => ps |> List.rev |> trim_l |> List.rev
    };
  };

let edge_shape_of = (d: Direction.t, ps: t): option(Nib.Shape.t) => {
  let trimmed = trim_whitespace(d, ps);
  switch (d, ListUtil.hd_opt(trimmed), ListUtil.last_opt(trimmed)) {
  | (Right, _, Some(p)) => p |> Piece.shapes |> Option.map(snd)
  | (Left, Some(p), _) => p |> Piece.shapes |> Option.map(fst)
  | _ => None
  };
};

let edge_direction_of = (d: Direction.t, ps: t): option(Direction.t) =>
  Option.map(Nib.Shape.absolute(d), edge_shape_of(d, ps));

let rec serialize = (seg: t) =>
  seg
  |> List.map(
       fun
       | (Piece.Whitespace(_) | Grout(_) | Tile({shards: [_], _})) as p => [
           p,
         ]
       | Tile(t) => {
           let shards =
             List.map(
               Tile.to_piece,
               Tile.split_shards(t.id, t.label, t.mold, t.shards),
             );
           let children = List.map(serialize, t.children);
           Aba.mk(shards, children)
           |> Aba.join(s => [s], Fun.id)
           |> List.concat;
         },
     )
  |> List.concat;
