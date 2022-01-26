open Sexplib.Std;

[@deriving sexp]
type t = list(Segment.t);

// let is_balanced = (segments: t): bool =>
//   Segment.is_whole_any(
//     Parser.parse_selection(Right, List.concat(segments)),
//   );
let is_balanced = _ => failwith("Backpack.is_balanced");

let total_segment = (up: t): Segment.t => Segment.concat(up);

let extend = (side: Util.Direction.t, segment, up): t =>
  switch (side) {
  | Left => [segment, ...up]
  | Right => up @ [segment]
  };

let pop = (side: Util.Direction.t, up: t): option((Segment.t, t)) =>
  switch (side) {
  | Left =>
    switch (up) {
    | [] => None
    | [popped, ...up] => Some((popped, up))
    }
  | Right =>
    open Util.OptUtil.Syntax;
    let+ (up, popped) = Util.ListUtil.split_last_opt(up);
    (popped, up);
  };
