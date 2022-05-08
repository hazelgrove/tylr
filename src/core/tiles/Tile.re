open Util;

exception Empty_disassembly;
exception Ambiguous_molds;
exception Invalid_mold;

include Base.Tile;

module Label = {
  include Label;
  // check if can be moved to Base
  exception Empty_label;

  let length: t => int = List.length;

  let rev: t => t = List.rev;

  let hd_tl = (lbl: t): (Token.t, list(Token.t)) =>
    switch (lbl) {
    | [] => raise(Empty_label)
    | [hd, ...tl] => (hd, tl)
    };
};

let to_piece = t => Base.Piece.Tile(t);

let sorted_children = ({mold, children, _}: t) =>
  switch (List.combine(mold.sorts.in_, children)) {
  | exception (Invalid_argument(_)) => raise(Invalid_mold)
  | r => r
  };

let remold = (t: t): list(t) =>
  Molds.get(t.label) |> List.map(mold => {...t, mold});

// postcond: output segment is nonempty
// TODO double shard indices
let disassemble =
    (from: Direction.t, {id, label, mold, children}: t): Base.Segment.t => {
  let r = from == Right;
  let shards = List.map(Shard.to_piece, Shard.mk_s(id, label, mold));
  let (hd, tl) =
    switch (ListUtil.rev_if(r, shards)) {
    | [] => raise(Label.Empty_label)
    | [hd, ...tl] => (hd, tl)
    };
  let children = List.map(ListUtil.rev_if(r), ListUtil.rev_if(r, children));
  List.combine(children, tl)
  |> ListUtil.flat_map(((child, shard)) => child @ [shard])
  |> List.cons(hd);
};

let default_mold =
    (_form: Label.t, _sibling: Sort.t, _ancestor: Sort.t): Mold.t =>
  failwith("todo Tile.default_mold");

// let reshape = (tile: intact) =>
//   assignable_molds(Intact.label(tile))
//   |> List.filter((mold: Mold.t) => mold.sorts == tile.mold.sorts)
//   |> List.map(mold => {...tile, mold});

let pop = (from: Direction.t, tile: t): (Base.Piece.t, Base.Segment.t) =>
  tile
  |> disassemble(from)
  |> ListUtil.split_first_opt
  |> OptUtil.get_or_raise(Empty_disassembly);

let unique_mold = _ => failwith("todo unique_mold");

// probably no longer necessary given tile ids
module Match = {
  type tile = t;

  module Make = (O: Orientation.S) => {
    [@deriving show]
    type t = Aba.t(Shard.t, Base.Segment.t);

    let init = s => (s, []);

    let shards = Aba.get_a;

    let id = ((hd, _): t) => hd.tile_id;

    let label = ((hd, _): t) => snd(hd.label);

    let length = m => List.length(Aba.get_a(m));

    let children = Aba.get_b;

    let extend = (s: Shard.t, seg: Base.Segment.t, (hd, tl): t) =>
      Shard.is_next(O.d, s, hd) ? Some((s, [(seg, hd), ...tl])) : None;

    let flatten = (m: t): Base.Segment.t =>
      m
      |> Aba.map_to_list(s => [Base.Piece.Shard(s)], Fun.id)
      |> List.flatten;

    let complete = (m: t): option(tile) => {
      let id = id(m);
      let label = label(m);
      let molds =
        switch (Shard.consistent_molds(shards(m))) {
        | [] =>
          // this should only happen upon construct/destruct,
          // in which case everything will be subsequently remolded
          Molds.get(label)
        | [_, ..._] as molds => molds
        };
      assert(molds != []);
      let mold = List.hd(molds);
      length(m) == Label.length(label)
        ? {
          let children =
            ListUtil.(
              rev_if(
                O.d == Left,
                List.map(rev_if(O.d == Left), children(m)),
              )
            );
          Some(Base.Tile.{id, label, mold, children});
        }
        : None;
    };
  };
};
