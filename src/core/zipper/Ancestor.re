open Util;

[@deriving show]
type step = int;

[@deriving show]
type t = {
  id: Id.t,
  label: Tile.Label.t,
  mold: Mold.t,
  children: ListFrame.t(Segment.t),
};

// TODO flatten with shard indices
let step = (frame: t): step => {
  let (prefix, _) = frame.children;
  List.length(prefix);
};

let remold = (a: t): list(t) =>
  Molds.get(a.label) |> List.map(mold => {...a, mold});

let sort = (frame: t): Sort.t => {
  assert(
    step(frame) >= 0 && step(frame) < List.length(frame.mold.sorts.in_),
  );
  List.nth(frame.mold.sorts.in_, step(frame));
};

let sort_rank = (a: t, (s_l, s_r): (Sort.t, Sort.t)) => {
  let s = a.mold.sorts.out;
  Bool.to_int(s != s_l) + Bool.to_int(s != s_r);
};

let disassemble =
    ({id, label, mold, children: (kids_l, kids_r)}: t): Siblings.t => {
  let (shards_l, shards_r) =
    Shard.mk_s(id, label, mold)
    |> List.map(Shard.to_piece)
    |> ListUtil.split_n(List.length(kids_l) + 1);
  let flatten = (shards, kids) =>
    List.flatten(ListUtil.map_alt(p => [p], Fun.id, shards, kids));
  (flatten(List.rev(shards_l), kids_l), flatten(shards_r, kids_r));
};

// module Match = {
//   type ancestor = t;
//   type t = (Tile.Match.t, Tile.Match.t);

//   let id = ((pre, _): t) => Tile.Match.id(pre);

//   let shards = ((pre, suf): t) =>
//     List.rev(Tile.Match.shards(pre)) @ Tile.Match.shards(suf);

//   let label = ((_, suf)) => Tile.Match.label(suf);

//   let length = ((pre, suf)) => Tile.Match.(length(pre) + length(suf));

//   let children = ((pre, suf)) => (
//     List.rev(Tile.Match.children(List.map(List.rev, pre))),
//     Tile.Match.children(suf),
//   );

//   let mold = (m: t) => {
//     let molds =
//       switch (Shard.consistent_molds(shards(m))) {
//       | [] =>
//         // this should only happen upon construct/destruct,
//         // in which case everything will be subsequently remolded
//         Molds.get(label(m))
//       | [_, ..._] as molds => molds
//       };
//     assert(molds != []);
//     List.hd(molds);
//   };

//   let complete = (m: t): option(ancestor) => {
//     let id = id(m);
//     let label = label(m);
//     let mold = mold(m);
//     let children = children(m);
//     length(m) == Tile.Label.length(label)
//       ? Some({id, label, mold, children}) : None;
//   };
// };

module Match = {
  module Prefix = Tile.Match.Make(Orientation.L);
  module Suffix = Tile.Match.Make(Orientation.R);

  type ancestor = t;
  type t = (Prefix.t, Suffix.t);

  let id = ((pre, _): t) => Prefix.id(pre);

  let shards = ((pre, suf): t) =>
    List.rev(Prefix.shards(pre)) @ Suffix.shards(suf);

  let label = ((_, suf)) => Suffix.label(suf);

  let length = ((pre, suf)) => Prefix.length(pre) + Suffix.length(suf);

  let children = ((pre, suf)) => (
    Prefix.children(pre),
    Suffix.children(suf),
  );

  let join = ((pre, suf): t) => (Prefix.join(pre), Suffix.join(suf));

  let complete = (m: t): option(ancestor) => {
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
    length(m) == Tile.Label.length(label)
      ? Some({id, label, mold, children: children(m)}) : None;
  };
};
