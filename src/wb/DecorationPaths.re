open Sexplib.Std;
open Util;
open Cor;
open OptUtil.Syntax;

[@deriving sexp]
type t = {
  caret: option((CaretMode.t, Path.t, list(Path.caret_step))),
  outer_cousins: list(Path.steps),
  inner_cousins: list(Path.steps),
};

let empty = {caret: None, outer_cousins: [], inner_cousins: []};

let mk_inner_cousins =
    (steps: Path.steps, subject: Subject.t): list(Path.steps) => {
  let of_selems = (offset, selems) =>
    selems
    |> List.mapi((i, selem) =>
         Path.get_child_steps(selem)
         |> List.map(step => steps @ [(i + offset, step)])
       )
    |> List.flatten;
  switch (subject) {
  | Pointing(sframe) => of_selems(0, ListFrame.to_list(sframe))
  | Selecting(selection, (prefix, suffix)) =>
    let prefix = of_selems(0, List.rev(prefix));
    let suffix =
      of_selems(List.length(prefix) + List.length(selection), suffix);
    prefix @ suffix;
  | Restructuring(selection, sframe) =>
    Selection.is_whole_any(selection)
      ? of_selems(0, ListFrame.to_list(sframe)) : []
  };
};

let mk_outer_cousins = (steps: Path.steps, frame: Frame.t): list(Path.steps) => {
  switch (
    Path.get_child_steps_of_frame(frame),
    ListUtil.split_last_opt(steps),
  ) {
  | (None, _)
  | (_, None) => []
  | (Some((_, others)), Some((leading_steps, _))) =>
    let (steps, (selem_step, _)) = ListUtil.split_last(steps);
    let same_tile_cousins =
      others |> List.map(child_step => steps @ [(selem_step, child_step)]);
    [leading_steps, ...same_tile_cousins];
  };
};

let mk = ((subject, frame): Zipper.t) => {
  let sframe =
    switch (subject) {
    | Pointing(sframe)
    | Selecting(_, sframe)
    | Restructuring(_, sframe) => sframe
    };
  let (steps, _) as path = Path.mk(sframe, frame);
  let outer_cousins = mk_outer_cousins(steps, frame);
  let inner_cousins = mk_inner_cousins(steps, subject);
  let caret =
    switch (subject) {
    | Pointing(sframe) =>
      Some((
        CaretMode.Pointing,
        path,
        ListUtil.range(List.length(ListFrame.to_list(sframe)) + 1),
      ))
    | Selecting(_, (prefix, _)) =>
      Some((Selecting, path, ListUtil.range(List.length(prefix) + 1)))
    | Restructuring(selection, (prefix, suffix) as sframe) =>
      let range =
        if (Selection.is_whole_any(selection)) {
          ListUtil.range(List.length(ListFrame.to_list(sframe)) + 1);
        } else {
          let (tiles_pre, prefix) =
            ListUtil.take_while(Selem.is_tile, prefix);
          let (tiles_suf, _) = ListUtil.take_while(Selem.is_tile, suffix);
          ListUtil.range(
            ~lo=List.length(prefix),
            List.(
              length(prefix) + length(tiles_pre) + length(tiles_suf) + 1
            ),
          );
        };
      Some((Restructuring(selection), path, range));
    };
  {caret, outer_cousins, inner_cousins};
};

// let neighbors = ((subject, frame): Zipper.t): Range.t => {
//   let caret_steps =
//     switch (subject) {
//     | Pointing((prefix, suffix)) => (
//         0,
//         List.length(prefix) + List.length(suffix),
//       )
//     | Selecting(_, (prefix, _)) => (0, List.length(prefix))
//     | Restructuring(selection, (prefix, suffix)) =>
//       if (Selection.is_whole_any(selection)) {
//         (0, List.length(prefix) + List.length(suffix));
//       } else {
//         let (tiles_pre, prefix) = ListUtil.take_while(Selem.is_tile, prefix);
//         let (tiles_suf, _) = ListUtil.take_while(Selem.is_tile, suffix);
//         List.(
//           length(prefix),
//           length(prefix) + length(tiles_pre) + length(tiles_suf),
//         );
//       }
//     };
//   (Path.mk_steps(frame), caret_steps);
// };

// let mk = ((subject, frame) as zipper: Zipper.t) => {
//   // let caret = CaretPath.mk(zipper);
//   let (caret, anchors) =
//     switch (subject) {
//     | Pointing(sframe) =>
//       let path = Path.mk(sframe, frame);
//       (Some((path, CaretMode.Pointing)), Some(Range.empty(path)));
//     | Selecting(selection, sframe) =>
//       let path = Path.mk(sframe, frame);
//       (
//         Some((path, Selecting)),
//         Some(Range.mk(path, List.length(selection))),
//       );
//     | Restructuring(selection, sframe) =>
//       let path = Path.mk(sframe, frame);
//       (Some((path, Restructuring(selection))), Some(Range.empty(path)));
//     };
//   let neighbors = Some(neighbors(zipper));
//   {caret, anchors, neighbors};
// };

let take_two_step = (two_step: Path.two_step, paths: t): t => {
  let step_or_prune =
    fun
    | [two_step', ...steps] when two_step' == two_step => Some(steps)
    | _ => None;
  let caret = {
    let* (mode, (steps, caret_step), siblings) = paths.caret;
    let+ steps = step_or_prune(steps);
    (mode, (steps, caret_step), siblings);
  };
  let inner_cousins = List.filter_map(step_or_prune, paths.inner_cousins);
  let outer_cousins = List.filter_map(step_or_prune, paths.outer_cousins);
  {caret, inner_cousins, outer_cousins};
};
// let take_two_step = (two_step: Path.two_step, paths: t): t => {
//   let* (path, mode) = paths;
//   switch (path) {
//   | ([two_step', ...steps], caret_step) when two_step' == two_step =>
//     Some(((steps, caret_step), mode))
//   | _ => None
//   };
// };

// let current =
//     (caret_step: Path.caret_step, paths: t): list(DecorationShape.t) => {
//   let is_caret =
//     switch (paths.caret) {
//     | Some((([], caret_step'), mode)) when caret_step == caret_step' =>
//       Some(mode)
//     | _ => None
//     };
//   let is_anchor =
//     switch (paths.anchors) {
//     | Some(([], (l, r))) when caret_step == l || caret_step == r => true
//     | _ => false
//     };
//   switch (is_caret) {
//   | Some(mode) => [Caret(mode), Anchor]
//   | None =>
//     if (is_anchor) {
//       [Anchor];
//     } else {
//       switch (paths.neighbors) {
//       | Some(([], (l, r))) when l <= caret_step && caret_step <= r => [
//           Neighbor,
//         ]
//       | _ => []
//       };
//     }
//   };
// };
let current =
    (caret_step: Path.caret_step, paths: t): (list(DecorationShape.t) as 'ds) => {
  let caret_sibling_ds: 'ds =
    switch (paths.caret) {
    | Some((mode, ([], caret_step'), _)) when caret_step == caret_step' => [
        Caret(mode),
        Sibling,
      ]
    | Some((_, ([], _), siblings)) when List.mem(caret_step, siblings) => [
        Sibling,
      ]
    | _ => []
    };
  let outer_cousin_ds: 'ds =
    List.mem([], paths.outer_cousins) ? [OuterCousin] : [];
  let inner_cousin_ds: 'ds =
    List.mem([], paths.inner_cousins) ? [InnerCousin] : [];
  List.concat([outer_cousin_ds, caret_sibling_ds, inner_cousin_ds]);
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
