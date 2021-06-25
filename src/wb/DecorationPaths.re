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
