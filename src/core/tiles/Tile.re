open Util;

exception Empty_disassembly;
exception Ambiguous_molds;

include Base.Tile;

module Label = {
  include Label;
  // check if can be moved to Base
  exception Empty_label;

  let len: t => int = List.length;

  let rev: t => t = List.rev;

  let hd_tl = (lbl: t): (Token.t, list(Token.t)) =>
    switch (lbl) {
    | [] => raise(Empty_label)
    | [hd, ...tl] => (hd, tl)
    };
};

let to_piece = t => Base.Piece.Tile(t);

// postcond: output segment is nonempty
// TODO double shard indices
let disassemble =
    (from: Direction.t, {label, mold, children}: t): Base.Segment.t => {
  let r = from == Right;
  let shards = List.map(Shard.to_piece, Shard.mk_s(label, mold));
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
