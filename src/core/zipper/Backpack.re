[@deriving show]
type t = list(Selection.t);

let empty = [];

let left_to_right: t => list(Selection.t) =
  List.fold_left(
    (l2r, sel: Selection.t) =>
      switch (sel.focus) {
      | Left => [sel, ...l2r]
      | Right => l2r @ [sel]
      },
    [],
  );

let is_balanced = (bp: t) =>
  left_to_right(bp)
  |> List.map((s: Selection.t) => s.content)
  |> Segment.concat
  |> Segment.reassemble
  |> Segment.is_balanced;

let push = sel => Selection.is_empty(sel) ? Fun.id : List.cons(sel);

let push_s: (list(Selection.t), t) => t = List.fold_right(push);

let pop = Util.ListUtil.split_first_opt;

let remove_matching = (ss: list(Shard.t), bp: t) =>
  List.fold_left(
    (bp, s) =>
      bp
      |> List.map(Selection.map(Segment.remove_matching(s)))
      |> List.filter_map(
           fun
           | Selection.{content: [], _} => None
           | sel => Some(sel),
         ),
    bp,
    ss,
  );

// let pick_up = (side: Util.Direction.t, tiles: list(Tiles.t), backpack): t => {
//   let tiles = List.filter((!=)(Tiles.empty), tiles);
//   switch (side) {
//   | Left => tiles @ backpack
//   | Right => backpack @ tiles
//   };
// };

// let put_down = (side: Util.Direction.t, backpack: t): option((Tiles.t, t)) =>
//   switch (side) {
//   | Left =>
//     switch (backpack) {
//     | [] => None
//     | [put_down, ...backpack] => Some((put_down, backpack))
//     }
//   | Right =>
//     open Util.OptUtil.Syntax;
//     let+ (backpack, put_down) = Util.ListUtil.split_last_opt(backpack);
//     (put_down, backpack);
//   };
