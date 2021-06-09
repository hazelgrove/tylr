open Util;
open Cor;
open OptUtil.Syntax;

type t = {
  caret: option((Path.t, CaretMode.t)),
  anchors: list(Range.t),
  neighbors: (option(Path.t), option(Path.t)),
};

let empty = {caret: None, anchors: [], neighbors: (None, None)};

let neighbors =
    ((subject, frame): Zipper.t): (option(Path.t), option(Path.t)) => {
  let sframe =
    switch (subject) {
    | Pointing(sframe)
    | Restructuring(_, sframe) => sframe
    | Selecting(selection, (prefix, suffix)) => (prefix, selection @ suffix)
    };
  let move = d => {
    let+ (sframe, frame) = Path.move_zipper(d, sframe, frame);
    Path.mk(sframe, frame);
  };
  (move(Left), move(Right));
};

let mk = ((subject, frame) as zipper: Zipper.t) => {
  // let caret = CaretPath.mk(zipper);
  let (caret, anchors) =
    switch (subject) {
    | Pointing(sframe) =>
      let path = Path.mk(sframe, frame);
      (Some((path, CaretMode.Pointing)), [Range.empty(path)]);
    | Selecting(selection, sframe) =>
      let path = Path.mk(sframe, frame);
      (Some((path, Selecting)), [Range.mk(path, List.length(selection))]);
    | Restructuring(selection, sframe) =>
      let path = Path.mk(sframe, frame);
      (Some((path, Restructuring(selection))), [Range.empty(path)]);
    };
  let neighbors = CaretPath.neighbors(zipper);
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
  let anchors = List.filter_map(step_or_prune, paths.anchors);
  let neighbors =
    paths.neighbors |> TupleUtil.map2(OptUtil.and_then(step_or_prune));
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
    paths.anchors
    |> List.exists(
         fun
         | ([], (l, r)) when caret_step == l || caret_step == r => true
         | _ => false,
       );
  switch (is_caret) {
  | Some(mode) => [Caret(mode), Anchor]
  | None =>
    if (is_anchor) {
      [Anchor];
    } else {
      switch (paths.neighbors) {
      | (Some(([], step)), _) when caret_step == step => [Neighbor]
      | (_, Some(([], step))) when caret_step == step => [Neighbor]
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
