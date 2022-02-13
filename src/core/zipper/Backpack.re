open Sexplib.Std;

[@deriving sexp]
type t = list(Segment.t);

let empty = [];

let is_balanced = (segments: t) =>
  Segment.is_intact(Parser.reassemble_segment(Segment.concat(segments)));

let total_segment = (up: t): Segment.t => Segment.concat(up);

let pick_up =
    (side: Util.Direction.t, segments: list(Segment.t), backpack): t => {
  let segments = List.filter((!=)(Segment.empty), segments);
  switch (side) {
  | Left => segments @ backpack
  | Right => backpack @ segments
  };
};

let put_down = (side: Util.Direction.t, backpack: t): option((Segment.t, t)) =>
  switch (side) {
  | Left =>
    switch (backpack) {
    | [] => None
    | [put_down, ...backpack] => Some((put_down, backpack))
    }
  | Right =>
    open Util.OptUtil.Syntax;
    let+ (backpack, put_down) = Util.ListUtil.split_last_opt(backpack);
    (put_down, backpack);
  };

let remove = (shards: list(Shard.Labeled.t), backpack: t) =>
  List.fold_left(
    (backpack, shard) => List.map(Tiles.remove_matching(shard), backpack),
    backpack,
    to_remove,
  );
