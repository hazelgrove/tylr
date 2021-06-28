open Sexplib.Std;
open Util;
open Cor;
open OptUtil.Syntax;

[@deriving sexp]
type t = {
  // TODO rename to anchors
  caret: option(Path.range),
  siblings: option((Path.steps, list(Path.caret_step))),
};

let empty = {caret: None, siblings: None};

let mk = ((subject, _) as zipper: Zipper.t) => {
  let (steps, _) as caret_range = Path.mk_range(zipper);
  let sibling_range =
    switch (subject) {
    | Pointing(sframe) =>
      ListUtil.range(List.length(ListFrame.to_list(sframe)) + 1)
    | Selecting(selection, (prefix, suffix)) =>
      let len_pre = List.length(prefix);
      let len_sel = List.length(selection);
      let len_suf = List.length(suffix);
      ListUtil.range(len_pre + 1)
      @ ListUtil.range(~lo=len_pre + len_sel, len_pre + len_sel + len_suf + 1);
    | Restructuring(selection, (prefix, suffix) as rframe) =>
      if (Selection.is_whole_any(selection)) {
        ListUtil.range(List.length(ListFrame.to_list(rframe)) + 1);
      } else {
        let (tiles_pre, prefix) = ListUtil.take_while(Selem.is_tile, prefix);
        let (tiles_suf, _) = ListUtil.take_while(Selem.is_tile, suffix);
        ListUtil.range(
          ~lo=List.length(prefix),
          List.(length(prefix) + length(tiles_pre) + length(tiles_suf) + 1),
        );
      }
    };
  {caret: Some(caret_range), siblings: Some((steps, sibling_range))};
};

let take_two_step = (two_step: Path.two_step, paths: t): t => {
  let step_or_prune =
    fun
    | [two_step', ...steps] when two_step' == two_step => Some(steps)
    | _ => None;
  let caret = {
    let* (steps, caret_range) = paths.caret;
    let+ steps = step_or_prune(steps);
    (steps, caret_range);
  };
  let siblings = {
    let* (steps, siblings) = paths.siblings;
    let+ steps = step_or_prune(steps);
    (steps, siblings);
  };
  {caret, siblings};
};
let current =
    (
      ~caret_mode: option(CaretMode.t)=?,
      ~origin: int,
      (step, color): (Path.caret_step, Color.t),
      paths: t,
    )
    : (list(Dec.Profile.t) as 'ds) => {
  open Dec.Profile;
  let caret_ds =
    switch (paths.caret, caret_mode) {
    | (Some(([], (l, _))), Some(mode)) when step == l =>
      let restructuring_ds =
        switch (mode) {
        | Restructuring({selection, _}) => [
            RestructuringGenie({
              origin,
              length: Layout.length(Layout.mk_selection(color, selection)),
            }),
          ]
        | _ => []
        };
      [
        Caret({origin, color, mode}),
        CaretPos({origin, color, style: `Anchor}),
        ...restructuring_ds,
      ];
    | (Some(([], (_, r))), Some(_)) when step == r => [
        CaretPos({origin, color, style: `Anchor}),
      ]
    | _ => []
    };
  let anchor_ds =
    switch (paths.caret) {
    | Some(([], (l, r))) when step == l || step == r => [
        CaretPos({origin, color, style: `Anchor}),
      ]
    | _ => []
    };
  let bare_ds =
    switch (caret_mode) {
    | None => []
    | Some(Restructuring({selection, _}))
        when !Selection.is_whole_any(selection) =>
      []
    | _ => [CaretPos({origin, color, style: `Bare})]
    };
  let sibling_ds =
    switch (paths.siblings) {
    | Some(([], siblings)) when List.mem(step, siblings) => [
        CaretPos({origin, color, style: `Sibling}),
      ]
    | _ => []
    };
  List.concat([caret_ds, anchor_ds, bare_ds, sibling_ds]);
};
