open Util;

module Prefix = Affix.Make(Orientation.L);
module Suffix = Affix.Make(Orientation.R);

[@deriving show]
type t = (Prefix.t, Suffix.t);

let empty = (Prefix.empty, Suffix.empty);

let prepend = (d: Direction.t, seg: Segment.t, (l, r): t): t =>
  switch (d) {
  | Left => (List.rev(seg) @ l, r)
  | Right => (l, seg @ r)
  };

let concat = (sibss: list(t)): t =>
  sibss
  |> List.split
  |> PairUtil.map_fst(List.concat)
  |> PairUtil.map_snd(List.concat);

// let consistent_shards = ((pre, suf): t): bool => {
//   let shards_pre = Prefix.shards(pre);
//   let shards_suf = Suffix.shards(suf);
//   ListUtil.group_by(Shard.id, shards_pre @ shards_suf)
//   |> List.for_all(((_, shards)) => Shard.consistent_molds(shards) != []);
// };

let remold = ((pre, suf): t): list(t) => {
  open ListUtil.Syntax;
  let+ pre = Prefix.remold(pre)
  and+ suf = Suffix.remold(suf);
  (pre, suf);
};

let sorts = ((pre, suf): t, s: Sort.t) => (
  Prefix.sort(pre, s),
  Suffix.sort(suf, s),
);
let shapes = ((pre, suf): t) => (Prefix.shape(pre), Suffix.shape(suf));

let contains_matching = (t: Tile.t, (pre, suf): t) =>
  Prefix.contains_matching(t, pre) || Suffix.contains_matching(t, suf);

let push = (onto: Direction.t, p: Piece.t, (pre, suf): t): t =>
  switch (onto) {
  | Left => (Prefix.push(p, pre), suf)
  | Right => (pre, Suffix.push(p, suf))
  };

let pop = (from: Direction.t, (pre, suf): t): option((Piece.t, t)) =>
  OptUtil.Syntax.(
    switch (from) {
    | Left =>
      let+ (p, pre) = Prefix.pop(pre);
      (p, (pre, suf));
    | Right =>
      let+ (p, suf) = Suffix.pop(suf);
      (p, (pre, suf));
    }
  );

let incomplete_tiles = ((pre, suf): t) => (
  Prefix.incomplete_tiles(pre),
  Suffix.incomplete_tiles(suf),
);

let split_by_matching = (id: Id.t, (pre, suf): t) => (
  Prefix.split_by_matching(id, pre),
  Suffix.split_by_matching(id, suf),
);

let reassemble = ((pre, suf): t) => (
  Prefix.reassemble(pre),
  Suffix.reassemble(suf),
);

let sort_rank = ((pre, suf): t, s: Sort.t) =>
  Prefix.sort_rank(pre, s) + Suffix.sort_rank(suf, s);

let shape_rank = ((pre, suf): t) =>
  Prefix.shape_rank(pre) + Suffix.shape_rank(suf);

let regrout = ((pre, suf): t) => (
  Prefix.regrout(pre),
  Suffix.regrout(suf),
);

let neighbors: t => (option(Piece.t), option(Piece.t)) =
  ((l, r)) => (
    l == [] ? None : Some(List.hd(l)),
    r == [] ? None : Some(List.hd(r)),
  );

let has_space_neighbor: t => bool =
  siblings =>
    switch (neighbors(siblings)) {
    | (Some(p), _) when Piece.is_whitespace(p) => true
    | (_, Some(p)) when Piece.is_whitespace(p) => true
    | _ => false
    };
