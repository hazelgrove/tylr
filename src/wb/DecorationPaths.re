open Sexplib.Std;
open Util;
open Cor;
open OptUtil.Syntax;

[@deriving sexp]
type t = {
  caret: option((CaretMode.t, Path.t, list(Path.caret_step))),
  anchors: list(Path.t),
};

let empty = {caret: None, anchors: []};

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
  | Selecting(_, selection, (prefix, suffix)) =>
    let prefix = of_selems(0, List.rev(prefix));
    let suffix =
      of_selems(List.length(prefix) + List.length(selection), suffix);
    prefix @ suffix;
  | Restructuring(selection, sframe) =>
    Selection.is_whole_any(selection)
      ? of_selems(0, ListFrame.to_list(sframe)) : []
  };
};

let mk = ((subject, frame): Zipper.t) => {
  let sframe =
    switch (subject) {
    | Pointing(sframe)
    | Restructuring(_, sframe) => sframe
    | Selecting(side, selection, (prefix, suffix)) =>
      switch (side) {
      | Left => (prefix, selection @ suffix)
      | Right => (List.rev(selection) @ prefix, suffix)
      }
    };
  let (steps, caret_step) as path = Path.mk(sframe, frame);
  let caret =
    switch (subject) {
    | Pointing(sframe) =>
      Some((
        CaretMode.Pointing,
        path,
        ListUtil.range(List.length(ListFrame.to_list(sframe)) + 1),
      ))
    | Selecting(_, selection, (prefix, suffix)) =>
      let len_pre = List.length(prefix);
      let len_sel = List.length(selection);
      let len_suf = List.length(suffix);
      let siblings =
        ListUtil.range(len_pre + 1)
        @ ListUtil.range(
            ~lo=len_pre + len_sel,
            len_pre + len_sel + len_suf + 1,
          );
      Some((Selecting, path, siblings));
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
  let anchors =
    switch (subject) {
    | Pointing(_)
    | Restructuring(_) => [path]
    | Selecting(side, selection, _) => [
        path,
        (
          steps,
          switch (side) {
          | Left => caret_step + List.length(selection)
          | Right => caret_step - List.length(selection)
          },
        ),
      ]
    };
  {caret, anchors};
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
  let anchors =
    paths.anchors
    |> List.filter_map(((steps, caret_step)) => {
         let+ steps = step_or_prune(steps);
         (steps, caret_step);
       });
  {caret, anchors};
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
  let anchor_ds: 'ds =
    List.mem(([], caret_step), paths.anchors) ? [Anchor] : [];
  List.concat([anchor_ds, caret_sibling_ds]);
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
