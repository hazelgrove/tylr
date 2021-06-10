open Sexplib.Std;
open Util;
open Cor;
open OptUtil.Syntax;

[@deriving sexp]
type t = {
  caret: option((Path.t, CaretMode.t)),
  anchors: option(Range.t),
  neighbors: option(Range.t),
};

let empty = {caret: None, anchors: None, neighbors: None};

let neighbors = ((subject, frame): Zipper.t): Range.t => {
  let caret_steps =
    switch (subject) {
    | Pointing((prefix, suffix)) => (
        0,
        List.length(prefix) + List.length(suffix),
      )
    | Selecting(_, (prefix, _)) => (0, List.length(prefix))
    | Restructuring(selection, (prefix, suffix)) =>
      if (Selection.is_whole_any(selection)) {
        (0, List.length(prefix) + List.length(suffix));
      } else {
        let (tiles_pre, prefix) = ListUtil.take_while(Selem.is_tile, prefix);
        let (tiles_suf, _) = ListUtil.take_while(Selem.is_tile, suffix);
        List.(
          length(prefix),
          length(prefix) + length(tiles_pre) + length(tiles_suf),
        );
      }
    };
  (Path.mk_steps(frame), caret_steps);
};

let mk = ((subject, frame) as zipper: Zipper.t) => {
  // let caret = CaretPath.mk(zipper);
  let (caret, anchors) =
    switch (subject) {
    | Pointing(sframe) =>
      let path = Path.mk(sframe, frame);
      (Some((path, CaretMode.Pointing)), Some(Range.empty(path)));
    | Selecting(selection, sframe) =>
      let path = Path.mk(sframe, frame);
      (
        Some((path, Selecting)),
        Some(Range.mk(path, List.length(selection))),
      );
    | Restructuring(selection, sframe) =>
      let path = Path.mk(sframe, frame);
      (Some((path, Restructuring(selection))), Some(Range.empty(path)));
    };
  let neighbors = Some(neighbors(zipper));
  {caret, anchors, neighbors};
};

let take_two_step = (two_step: Path.two_step, paths: t): t => {
  let step_or_prune =
    fun
    | ([two_step', ...steps], a) when two_step' == two_step =>
      Some((steps, a))
    | _ => None;
  let caret = {
    let* (path, mode) = paths.caret;
    let+ path = step_or_prune(path);
    (path, mode);
  };
  let anchors = Option.bind(paths.anchors, step_or_prune);
  let neighbors = Option.bind(paths.neighbors, step_or_prune);
  {caret, anchors, neighbors};
};

let current =
    (caret_step: Path.caret_step, paths: t): list(DecorationShape.t) => {
  let is_caret =
    switch (paths.caret) {
    | Some((([], caret_step'), mode)) when caret_step == caret_step' =>
      Some(mode)
    | _ => None
    };
  let is_anchor =
    switch (paths.anchors) {
    | Some(([], (l, r))) when caret_step == l || caret_step == r => true
    | _ => false
    };
  switch (is_caret) {
  | Some(mode) => [Caret(mode), Anchor]
  | None =>
    if (is_anchor) {
      [Anchor];
    } else {
      switch (paths.neighbors) {
      | Some(([], (l, r))) when l <= caret_step && caret_step <= r => [
          Neighbor,
        ]
      | _ => []
      };
    }
  };
};

// let mk = ((subject, _) as zipper: Zipper.t): t => {
//   let caret =
//     switch (subject) {
//     | Pointing(_) => Pointing
//     | Selecting(_) => Selecting
//     | Restructuring(selection, _) =>
//       Restructuring(
//         Layout.(
//           pad(
//             annot(
//               Selected,
//               spaces(mk_selection(~style=Selected, selection)),
//             ),
//           )
//         ),
//       )
//     };
//   let caret_path = Path.of_zipper(zipper);
//   let neighbor_l =
//     zipper |> Action.perform(Move(Left)) |> Option.map(Path.of_zipper);
//   let neighbor_r =
//     zipper |> Action.perform(Move(Right)) |> Option.map(Path.of_zipper);
//   {caret: (caret_path, caret), caret_neighbors: (neighbor_l, neighbor_r)};
// };
