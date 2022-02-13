open Sexplib.Std;
open Util;

exception Ambiguous_molds;

module Map = {
  include Map.Make(Id);
};

[@deriving sexp]
type s = list(t)
and t =
  | Pieces(Aba.t(Piece.t, s))
  | Intact(intact)
and intact = {
  id: Id.t,
  mold: Mold.t,
  substance: Util.Aba.t(Token.t, s),
};

let label = (tile: intact): Label.t => Aba.get_a(tile.substance);

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
  assignable_molds(label(tile))
  |> List.filter((mold: Mold.t) => mold.sorts == tile.mold.sorts)
  |> List.map(mold => {...tile, mold});

// postcond: output list is nonempty
let disassemble = (tile: t): s =>
  switch (tile) {
  | Pieces(pieces) =>
    pieces
    |> Aba.map_to_list(
         shard => [Placeholder({substance: (shard, [])})],
         tiles => tiles,
       )
    |> List.concat
  | Intact(tile) =>
    let mk_shard = index => [
      Placeholder({
        substance: (
          Labeled({
            tile: (tile.id, label(tile)),
            index,
            nibs: nibs(~index, tile.mold),
          }),
          [],
        ),
      }),
    ];
    tile.substance
    |> Aba.mapi_a((index, _token) => index)
    |> Aba.map_to_list(mk_shard, Fun.id)
    |> List.concat;
  };

module Frame = {};

let rec split = (tiles: s): option((s, Shard.placeholder, s)) =>
  switch (tiles) {
  | [] => None
  | [Placeholder({substance: (Placeholder(p), [])}), ...tiles] =>
    Some(([], p, tiles))
  | [tile, ...tiles] =>
    split(tiles)
    |> Option.map(((pre, p, suf)) => ([tile, ...pre], p, suf))
  };

// note: assumes input shards are matching and in order,
// and that overall substance is connected
let mk =
    (~reassemble: s => s, (shard, _) as tile_split: Aba.t(Shard.labeled, s))
    : t => {
  let (id, label) = shard.tile;
  if (List.length(Aba.get_a(tile_split)) == Tile.Label.len(label)) {
    let mold = unique_mold(Aba.get_a(tile_split));
    let substance =
      tile_split |> Aba.map_a(Shard.label) |> Aba.map_b(reassemble);
    Labeled({id, mold, substance});
  } else {
    substance
    |> Aba.fold_right(
         shard => (shard, []),
         (shard, tiles, (hd, tl)) =>
           switch (split(tiles)) {
           | None => (shard, [(tiles, hd), ...tl])
           | Some((pre, p, suf)) => (
               shard,
               [(pre, Shard.Placeholder(p)), (suf, hd), ...tl],
             )
           },
       );
  };
};
