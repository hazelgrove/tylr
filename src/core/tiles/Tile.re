open Sexplib.Std;
open Util;

exception Ambiguous_molds;

module Map = {
  include Map.Make(Id);
};

module Intact = {
  [@deriving sexp]
  type t('child) = {
    id: Id.t,
    mold: Mold.t,
    substance: Util.Aba.t(Token.t, 'child),
  };

  let label = (tile: t(_)): Label.t => Aba.get_a(tile.substance);
};

[@deriving sexp]
type s = list(t)
and t =
  | Pieces(pieces)
  | Intact(intact)
and pieces = Aba.t(Piece.t, s)
and intact = Intact.t(s);

let of_piece = (piece: Piece.t) => Pieces((piece, []));
let of_grout = g => of_piece(Grout(g));

let is_intact =
  fun
  | Intact(_) => true
  | Pieces(_) => false;

let assignable_molds = (~l as _: option(Nib.t)=?, label: Label.t) => {
  open Mold;
  let s = Sorts.mk;
  switch (label) {
  | [t] when Token.is_num(t) => [mk_op(s(Exp))]
  | [t] when Token.is_var(t) => [mk_op(s(Pat)), mk_op(s(Exp))]
  | ["(", ")"] => [
      mk_op(s(~in_=[Pat], Pat)),
      mk_op(s(~in_=[Exp], Exp)),
    ]
  | ["λ", "{", "}"] => [mk_op(s(~in_=[Pat, Exp], Exp))]
  | ["!"] => [mk_post(Precedence.fact, s(Exp))]
  | ["[", "]"] => [mk_post(Precedence.ap, s(~in_=[Exp], Exp))]
  | ["*" | "/"] => [mk_bin(Precedence.mult, s(Exp))]
  | ["+" | "-"] => [mk_bin(Precedence.plus, s(Exp))]
  | [","] => [
      mk_bin(Precedence.prod, s(Pat)),
      mk_bin(Precedence.prod, s(Exp)),
    ]
  | ["?", ":"] => [mk_bin(Precedence.cond, s(~in_=[Exp], Exp))]
  | ["let", "=", "in"] => [
      mk_pre(Precedence.let_, s(~in_=[Pat, Exp], Exp)),
    ]
  | _ => []
  };
};

let default_mold =
    (_form: Label.t, _sibling: Sort.t, _ancestor: Sort.t): Mold.t =>
  failwith("todo Tile.default_mold");

let nibs: (~index: int=?, Mold.t) => Nibs.t =
  (~index as _=?, _) => failwith("todo Tile.nibs");

let reshape = (tile: intact) =>
  assignable_molds(Intact.label(tile))
  |> List.filter((mold: Mold.t) => mold.sorts == tile.mold.sorts)
  |> List.map(mold => {...tile, mold});

// postcond: output list is nonempty
let disassemble = (tile: t): s =>
  switch (tile) {
  | Pieces(pieces) =>
    pieces
    |> Aba.map_to_list(shard => [Pieces((shard, []))], tiles => tiles)
    |> List.concat
  | Intact(tile) =>
    let mk_shard = index => [
      Pieces((
        Shard({
          tile: (tile.id, Intact.label(tile)),
          index,
          nibs: nibs(~index, tile.mold),
        }),
        [],
      )),
    ];
    tile.substance
    |> Aba.mapi_a((index, _token) => index)
    |> Aba.map_to_list(mk_shard, Fun.id)
    |> List.concat;
  };

let rec split = (tiles: s): option((s, Grout.t, s)) =>
  switch (tiles) {
  | [] => None
  | [Pieces((Grout(g), [])), ...tiles] => Some(([], g, tiles))
  | [tile, ...tiles] =>
    split(tiles)
    |> Option.map(((pre, g, suf)) => ([tile, ...pre], g, suf))
  };

let unique_mold = _ => failwith("todo unique_mold");

// note: assumes input shards are matching and in order,
// and that overall substance is connected
let mk =
    (~reassemble: s => s, (shard, _) as tile_split: Aba.t(Shard.t, s)): t => {
  let (id, label) = shard.tile;
  if (List.length(Aba.get_a(tile_split)) == Label.len(label)) {
    let mold = unique_mold(Aba.get_a(tile_split));
    let substance =
      tile_split |> Aba.map_a(Shard.label) |> Aba.map_b(reassemble);
    Intact({id, mold, substance});
  } else {
    let pieces =
      tile_split
      |> Aba.fold_right(
           shard => (Piece.Shard(shard), []),
           (shard, tiles, (hd, tl)) =>
             switch (split(tiles)) {
             | None => (Piece.Shard(shard), [(tiles, hd), ...tl])
             | Some((pre, g, suf)) => (
                 Piece.Shard(shard),
                 [(pre, Piece.Grout(g)), (suf, hd), ...tl],
               )
             },
         );
    Pieces(pieces);
  };
};
