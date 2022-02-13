open Sexplib.Std;
open Util;

exception Ambiguous_molds;

module Id = {
  [@deriving sexp]
  type t = int;
  let compare = Int.compare;
};

module Map = {
  include Map.Make(Id);
};

module Label = {
  [@deriving sexp]
  type t = list(Token.t);

  let len: t => int = List.length;
};

module Shape = {
  [@deriving sexp]
  type t =
    | Op
    | Pre
    | Post
    | Bin;
};

module Sorts = {
  [@deriving sexp]
  type t = {
    out: Sort.t,
    in_: list(Sort.t),
  };
  let mk = (~in_=[], out) => {out, in_};
};

module Mold = {
  [@deriving sexp]
  type t = {
    precedence: Precedence.t,
    shape: Shape.t,
    sorts: Sorts.t,
  };

  let mk_op = sorts => {sorts, shape: Op, precedence: Precedence.max_p};
  let mk_pre = (precedence, sorts) => {sorts, precedence, shape: Pre};
  let mk_post = (precedence, sorts) => {sorts, precedence, shape: Post};
  let mk_bin = (precedence, sorts) => {sorts, precedence, shape: Bin};
};

module Shard = {
  [@deriving sexp]
  type index = int;
  [@deriving sexp]
  type labeled = {
    tile: (Id.t, Label.t),
    index: int,
    nibs: Nibs.t,
  };
  [@deriving sexp]
  type placeholder = {
    ids: list(Id.t),
    p: Precedence.t,
  };
  [@deriving sexp]
  type t =
    | Placeholder(placeholder)
    | Labeled(labeled);
};

[@deriving sexp]
type s = list(t)
and t =
  | Placeholder({substance: Aba.t(Shard.t, s)})
  | Labeled(labeled)
// and placeholder =
and labeled = {
  id: Id.t,
  mold: Mold.t,
  substance: Util.Aba.t(Token.t, s),
};

// module Placeholder = {
//   type t = placeholder;
// };
// module Labeled = {
//   type t = labeled;
// };

let label = (labeled: labeled): Label.t => Aba.get_a(labeled.substance);

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

let reshape = (labeled: labeled) =>
  assignable_molds(label(labeled))
  |> List.filter((mold: Mold.t) => mold.sorts == labeled.mold.sorts)
  |> List.map(mold => {...labeled, mold});

// postcond: output list is nonempty
let disassemble = (tile: t): s =>
  switch (tile) {
  | Placeholder(p) =>
    p.substance
    |> Aba.map_to_list(
         shard => [Placeholder({substance: (shard, [])})],
         tiles => tiles,
       )
    |> List.concat
  | Labeled(labeled) =>
    let mk_shard = index => [
      Placeholder({
        substance: (
          Labeled({
            tile: (labeled.id, label(labeled)),
            index,
            nibs: nibs(~index, labeled.mold),
          }),
          [],
        ),
      }),
    ];
    labeled.substance
    |> Aba.mapi_a((index, _token) => index)
    |> Aba.map_to_list(mk_shard, Fun.id)
    |> List.concat;
  };

module Frame = {
  [@deriving sexp]
  type step = int;

  module Labeled = {
    [@deriving sexp]
    type t = {
      id: Id.t,
      mold: Mold.t,
      substance: Util.Aba.Frame.B.t(Token.t, s),
    };

    let step = (frame: t): step => {
      let (prefix, _) = frame.substance;
      List.length(Util.Aba.get_b(prefix));
    };

    let sort = (frame: t): Sort.t =>
      List.nth(frame.mold.sorts.in_, step(frame));
    // let disassemble = (frame: t): (s, s) =>
    //   switch (frame) {
    //   | Placeholder({substance: (prefix, suffix)}) =>
    //   }

    let label = _ => failwith("todo Tile.Frame.label");
  };
  module Placeholder = {
    [@deriving sexp]
    type t = {substance: Util.Aba.Frame.B.t(Shard.t, s)};
  };

  [@deriving sexp]
  type t =
    | Placeholder(Placeholder.t)
    | Labeled(Labeled.t);
};

let rec split = (tiles: s): option((s, Shard.Placeholder.t, s)) =>
  switch (tiles) {
  | [] => None
  | [Tile.Placeholder((Placeholder(p), [])), ...tiles] =>
    Some(([], p, tiles))
  | [tile, ...tiles] =>
    split(tiles)
    |> Option.map(((pre, p, suf)) => ([tile, ...pre], p, suf))
  };

// note: assumes input shards are matching and in order,
// and that overall substance is connected
let mk = ((shard, _) as substance: Aba.t(Shard.Labeled.t, s)): t => {
  let (id, label) = shard.tile;
  if (List.length(Aba.get_a(tile_split)) == Tile.Label.len(label)) {
    let mold = unique_mold(Aba.get_a(tile_split));
    let substance =
      tile_split
      |> Aba.map_a(Shard.Labeled.label)
      |> Aba.map_b(reassemble_tiles);
    [Tile.Labeled({id, mold, substance}), ...last];
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
