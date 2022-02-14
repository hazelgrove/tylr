open Sexplib.Std;

[@deriving sexp]
type t = list(Tiles.t);

let empty = [];

let is_balanced = (tiles: t) =>
  Tiles.(is_intact(reassemble(concat(tiles))));

let total_segment = (up: t): Tiles.t => Tiles.concat(up);

let pick_up = (side: Util.Direction.t, tiles: list(Tiles.t), backpack): t => {
  let tiles = List.filter((!=)(Tiles.empty), tiles);
  switch (side) {
  | Left => tiles @ backpack
  | Right => backpack @ tiles
  };
};

let put_down = (side: Util.Direction.t, backpack: t): option((Tiles.t, t)) =>
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

let remove = (shards: list(Shard.t), backpack: t) =>
  List.fold_left(
    (backpack, shard) => List.map(Tiles.remove_matching(shard), backpack),
    backpack,
    shards,
  );
