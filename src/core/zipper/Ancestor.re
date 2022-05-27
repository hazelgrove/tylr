open Util;

[@deriving show]
type step = int;

[@deriving show]
type t = {
  id: Id.t,
  label: Label.t,
  mold: Mold.t,
  shards: ListFrame.t(int),
  children: ListFrame.t(Segment.t),
};

let zip = (child: Segment.t, {id, label, mold, shards, children}: t): Tile.t => {
  id,
  label,
  mold,
  shards: ListFrame.to_list(shards),
  children: ListFrame.to_list(~subject=[child], children),
};

// TODO flatten with shard indices
let step = (frame: t): step => {
  let (prefix, _) = frame.children;
  List.length(prefix);
};

let remold = (a: t): list(t) =>
  Molds.get(a.label) |> List.map(mold => {...a, mold});

// let sort = (frame: t): Sort.t => {
//   assert(step(frame) >= 0 && step(frame) < List.length(frame.mold.in_));
//   List.nth(frame.mold.in_, step(frame));
// };
let sort = _ => failwith("todo Ancestor.sort");

// let sort_rank = (a: t, (s_l, s_r): (Sort.t, Sort.t)) => {
//   let s = a.mold.out;
//   Bool.to_int(s != s_l) + Bool.to_int(s != s_r);
// };
let sort_rank = _ => failwith("todo Ancestor.sort_rank");

let disassemble =
    ({id, label, mold, shards, children: (kids_l, kids_r)}: t): Siblings.t => {
  let (shards_l, shards_r) =
    shards
    |> TupleUtil.map2(Tile.split_shards(id, label, mold))
    |> TupleUtil.map2(List.map(Tile.to_piece));
  let flatten = (shards, kids) =>
    List.flatten(ListUtil.map_alt(p => [p], Fun.id, shards, kids));
  (flatten(shards_l, kids_l), flatten(shards_r, kids_r));
};

let reassemble = (match_l: Aba.t(Tile.t, Segment.t) as 'm, match_r: 'm): t => {
  // TODO(d) bit hacky, need to do a flip/orientation pass
  let match_l = Aba.map_b(Segment.rev, match_l);
  let (t_l, t_r) = Tile.(reassemble(match_l), reassemble(match_r));
  assert(t_l.id == t_r.id);
  {
    id: t_l.id,
    label: t_l.label,
    mold: t_l.mold,
    shards: (t_l.shards, t_r.shards),
    children: (t_l.children, t_r.children),
  };
};

// module Match = {
//   module Prefix = Tile.Match.Make(Orientation.L);
//   module Suffix = Tile.Match.Make(Orientation.R);

//   type ancestor = t;
//   type t = (Prefix.t, Suffix.t);

//   let id = ((pre, _): t) => Prefix.id(pre);

//   let shards = ((pre, suf): t) =>
//     List.rev(Prefix.shards(pre)) @ Suffix.shards(suf);

//   let label = ((_, suf)) => Suffix.label(suf);

//   let length = ((pre, suf)) => Prefix.length(pre) + Suffix.length(suf);

//   let children = ((pre, suf)) => (
//     Prefix.children(pre),
//     Suffix.children(suf),
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

//   let join = ((pre, suf): t) => (Prefix.join(pre), Suffix.join(suf));

//   let complete = (m: t): option(ancestor) => {
//     let id = id(m);
//     let label = label(m);
//     let mold = mold(m);
//     length(m) == Tile.Label.length(label)
//       ? Some({id, label, mold, children: children(m)}) : None;
//   };
// };
