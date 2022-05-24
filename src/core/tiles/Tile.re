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

let shapes = (t: t) => {
  let (l, r) = Mold.nibs(t.mold);
  (l.shape, r.shape);
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

let pop = (from: Direction.t, tile: t): (Base.Piece.t, Base.Segment.t) =>
  tile
  |> disassemble(from)
  |> ListUtil.split_first_opt
  |> OptUtil.get_or_raise(Empty_disassembly);

let unique_mold = _ => failwith("todo unique_mold");

module Match = {
  type tile = t;

  module Make = (O: Orientation.S) => {
    [@deriving show]
    type t = Aba.t(Shard.t, Base.Segment.t);

    let id = (m: t) => Aba.hd(m).tile_id;

    let label = (m: t) => snd(Aba.hd(m).label);

    let shards: t => list(Shard.t) = Aba.get_as;
    // let children: t => list(Base.Segment.t) = Aba.get_bs;

    let length = (m: t) => List.length(shards(m));

    let mold = (m: t) => {
      let molds =
        switch (Shard.consistent_molds(shards(m))) {
        | [] =>
          // this should only happen upon construct/destruct,
          // in which case everything will be subsequently remolded
          Molds.get(label(m))
        | [_, ..._] as molds => molds
        };
      assert(molds != []);
      List.hd(molds);
    };

    let children = m =>
      List.map(ListUtil.rev_if(O.d == Left), Aba.get_bs(m));

    let join = (m: t): Base.Segment.t =>
      m |> Aba.join(s => [Shard.to_piece(s)], Fun.id) |> List.flatten;

    let complete = (m: t): option(tile) => {
      let id = id(m);
      let label = label(m);
      let mold = mold(m);
      length(m) == Label.length(label)
        ? {
          let children = ListUtil.rev_if(O.d == Left, children(m));
          Some(Base.Tile.{id, label, mold, children});
        }
        : None;
    };
  };
};
