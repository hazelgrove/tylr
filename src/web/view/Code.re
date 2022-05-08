open Virtual_dom.Vdom;
open Node;
open Core;
open Util;

let span_c = cls => span([Attr.class_(cls)]);

let rec text_view = (l: Layout.t): list(Node.t) => {
  switch (l) {
  | Text(s, ann) =>
    switch (ann) {
    | None
    | Ap
    | Space(_)
    | EmptyHole(_) => [text(s)]
    | Delim => [span_c("delim", [text(s)])]
    | DelimBold => [span_c("extra-bold-delim", [text(s)])]
    }
  | Cat(ls, _) => List.fold_left((ts, l) => ts @ text_view(l), [], ls)
  };
};

let range_profile = (ms: list(Layout.measured), i, j): Layout.measurement =>
  if (j == List.length(ms)) {
    print_endline("WARNING: TODO(andrew): handling of rightmost cursorpos");
    {origin: 0, length: 0};
  } else {
    assert(0 <= i && i <= j && j < List.length(ms));
    let (ith, jth) = (List.nth(ms, i), List.nth(ms, j));
    let origin = ith.measurement.origin;
    let length = jth.measurement.origin - origin;
    {origin, length};
  };

let get_closed_children_measurements =
    (mold: Mold.t, ms: list(Layout.measured)): list(Layout.measurement) => {
  List.map((==)(mold.sorts.out), mold.sorts.in_)
  |> ListUtil.p_indices((==)(true))
  |> List.map(idx => {
       let i = Layout.segment_idx(idx);
       assert(i >= 0 && i < List.length(ms));
       List.nth(ms, i).measurement;
     });
};

let get_open_children_measurements =
    (mold: Mold.t, ms: list(Layout.measured)): list(Layout.measurement) => {
  List.map((==)(mold.sorts.out), mold.sorts.in_)
  |> ListUtil.p_indices((==)(false))
  |> List.map(idx => {
       let i = Layout.segment_idx(idx);
       assert(i >= 0 && i < List.length(ms));
       List.nth(ms, i).measurement;
     });
};

let sel_piece_profile =
    (mold: Mold.t, measurement, ms: list(Layout.measured))
    : SelemDec.Profile.t => {
  print_endline("sel_piece_profile");
  print_endline(
    String.concat(
      " ",
      List.map(
        Layout.show_measurement,
        get_closed_children_measurements(mold, ms),
      ),
    ),
  );
  {
    measurement,
    color: Color.of_sort(mold.sorts.out),
    shape: Layout.piece_shape_of_mold(mold),
    style: Selected,
    open_children: get_open_children_measurements(mold, ms),
    closed_children: get_closed_children_measurements(mold, ms),
  };
};

let backpack_sel_view = ({focus: _, content}: Selection.t): t => {
  let l = Layout.of_segment(Exp, content); // TODO(andrew): Exp
  let text_view = text_view(l);
  div([Attr.classes(["code-text", "backpack-selection"])], text_view);
};

let selection_length = (s: Selection.t): int =>
  // TODO(andrew): Exp
  Layout.to_measured(Layout.of_segment(Exp, s.content)).measurement.length;

let genie_profile = (backpack: Backpack.t, origin: int): Layout.measurement => {
  length: backpack |> List.map(selection_length) |> List.fold_left((+), 0),
  origin,
};

let backpack_view =
    (~font_metrics: FontMetrics.t, ~origin, backpack: Backpack.t): Node.t => {
  let style =
    Printf.sprintf(
      "position: absolute; left: %fpx; top: %fpx;",
      (Float.of_int(origin) -. 0.) *. font_metrics.col_width,
      (-2.) *. font_metrics.row_height,
    );
  let selections_view =
    div(
      [Attr.create("style", style), Attr.classes(["backpack"])],
      List.map(backpack_sel_view, backpack),
    );
  let genie_view =
    RestructuringGenieDec.view(
      ~font_metrics,
      genie_profile(backpack, origin),
    );
  div([Attr.classes(["backpack"])], [selections_view, genie_view]);
};

let cat_decos =
    (
      ~font_metrics,
      ~backpack,
      ann: Layout.ann_cat,
      measurement: Layout.measurement,
      ms,
    ) =>
  switch (ann) {
  | Segment(Range(i, j)) =>
    let origin = measurement.origin;
    [
      SelectedBoxDec.view(~font_metrics, range_profile(ms, i, j)),
      CaretDec.simple_view(~font_metrics, origin),
      backpack_view(~font_metrics, ~origin, backpack),
    ];
  | Piece(_p, mold, InsideFocalSegment(Selected)) => [
      SelemDec.view(~font_metrics, sel_piece_profile(mold, measurement, ms)),
    ]
  | _ => []
  };

let text_decos = (~font_metrics, ann: Layout.ann_text, measurement) =>
  switch (ann) {
  | EmptyHole(mold) => [
      EmptyHoleDec.view(~font_metrics: FontMetrics.t, {measurement, mold}),
    ]
  | _ => []
  };

let rec deco_view:
  (~font_metrics: FontMetrics.t, ~backpack: Backpack.t, Layout.measured) =>
  list(t) =
  (~font_metrics, ~backpack, layout) =>
    switch (layout.layout) {
    | CatM(ms, ann) =>
      cat_decos(~font_metrics, ~backpack, ann, layout.measurement, ms)
      @ List.concat(List.map(deco_view(~font_metrics, ~backpack), ms))
    | TextM(_s, ann) => text_decos(~font_metrics, ann, layout.measurement)
    };

let view =
    (
      ~id=?,
      ~text_id=?,
      ~font_metrics,
      ~zipper: Zipper.t,
      ~filler=0,
      ~just_failed as _: option(FailedInput.t)=None,
      ~show_neighbor_tiles as _: bool=false,
      _dpaths,
    )
    : Node.t => {
  let backpack = zipper.backpack;
  let l = Layout.mk_zipper(zipper);
  let m = Layout.to_measured(l);
  let text_view = text_view(l);
  let deco_view = deco_view(~font_metrics, ~backpack, m);
  let with_id: option('a) => list(Attr.t) =
    fun
    | None => []
    | Some(id) => [Attr.id(id)];
  let filler =
    filler == 0
      ? []
      : [
        span(
          [Attr.class_("filler")],
          [text(String.concat("", List.init(filler, _ => Unicode.nbsp)))],
        ),
      ];
  let text =
    span(
      [Attr.class_("code-text"), ...with_id(text_id)],
      text_view @ filler,
    );
  div([Attr.classes(["code"]), ...with_id(id)], [text, ...deco_view]);
};

// let delete_actions =
//   fun
//   | (Subject.Pointing((prefix, suffix)), frame)
//   | (Selecting(_, [], (prefix, suffix)), frame) => {
//       let action = affix =>
//         switch (affix) {
//         | [Piece.Tile(tile), ..._] =>
//           Tile.is_leaf(tile) ? Some(`Remove) : Some(`Restructure)
//         | _ =>
//           switch (frame) {
//           | Frame.Exp(Root) => None
//           | _ => Some(`Restructure)
//           }
//         };
//       (action(prefix), action(suffix));
//     }
//   | (Selecting(_, selection, _), _) =>
//     Selection.is_whole_any(selection)
//       ? (Some(`Remove), Some(`Remove))
//       : (Some(`Restructure), Some(`Restructure))
//   | (Restructuring(_), _) => (Some(`Remove), Some(`Remove));

// let view_of_layout =
//     (
//       ~id=?,
//       ~text_id=?,
//       ~font_metrics,
//       ~zipper as _: option(Zipper.t)=?,
//       ~filler=0,
//       ~just_failed as _: option(FailedInput.t)=None,
//       ~show_neighbor_tiles: bool=false,
//       dpaths,
//       l,
//     )
//     : Node.t => {
//   let decs = deco_view(~font_metrics, Layout.to_measured(l));
//   // let delete_actions = Option.map(delete_actions, zipper);
//   // let caret_mode =
//   //   zipper
//   //   |> Option.map(
//   //        fun
//   //        | (Subject.Pointing(_), _) => CaretMode.Pointing
//   //        | (Selecting(side, selection, _), _) => Selecting(side, selection)
//   //        | (Restructuring(((_, selection, rest) as backpack, rframe)), _) => {
//   //            let at_restructurable_selection =
//   //              switch (rframe) {
//   //              | ([Selection(selection), ..._], _)
//   //                  when
//   //                    Option.is_some(Selection.is_restructurable(selection)) =>
//   //                true
//   //              | (_, [Selection(selection), ..._])
//   //                  when
//   //                    Option.is_some(Selection.is_restructurable(selection)) =>
//   //                true
//   //              | _ => false
//   //              };
//   //            let view_of_selection = (~with_box, selection) => {
//   //              let l = Layout.mk_selection(~frame_color=Selected, selection);
//   //              let len = List.length(selection);
//   //              let dpaths =
//   //                DecPaths.mk(
//   //                  ~caret=([], (0, len)),
//   //                  ~selection_bars=[([], (0, len))],
//   //                  ~selection_boxes=with_box ? [([], (0, len))] : [],
//   //                  ~filtered_pieces=([], ListUtil.range(len)),
//   //                  (),
//   //                );
//   //              view_of_layout(~font_metrics, dpaths, l);
//   //            };
//   //            let selection = view_of_selection(~with_box=false, selection);
//   //            let rest = List.map(view_of_selection(~with_box=true), rest);
//   //            Restructuring({
//   //              at_restructurable_selection,
//   //              backpack,
//   //              view: (selection, rest),
//   //            });
//   //          },
//   //      );
//   let with_cls = cls => Node.span([Attr.classes([cls])]);
//   let rec go =
//           (~piece_step=?, ~indent=0, ~origin=0, dpaths, l: Layout.t)
//           : (list(Node.t), list(Node.t)) => {
//     switch (l) {
//     | Text(s, text_ann) =>
//       let t = [Node.text(s)];
//       switch (text_ann) {
//       | None => (t, [])
//       | Delim => ([with_cls("delim", t)], [])
//       | DelimBold => ([with_cls("extra-bold-delim", t)], [])
//       | Ap => (
//           [with_cls("ap", t)],
//           [ApDec.view(~font_metrics, {origin, length: 1})],
//         )
//       | Space(_step, _color) =>
//         // DecPaths.current_space(
//         //   ~delete_actions?,
//         //   ~caret_mode?,
//         //   ~just_failed,
//         //   ~measurement={origin, length: 1},
//         //   (step, color),
//         //   dpaths,
//         // )
//         // |> List.map(Dec.view(~font_metrics))
//         // |> add_decorations
//         (t, [])
//       | EmptyHole(color, _tip) =>
//         let m: Layout.measurement = {origin, length: 1};
//         (
//           t,
//           [
//             EmptyHoleDec.view(
//               ~font_metrics: FontMetrics.t,
//               {measurement: m, color},
//             ),
//           ],
//         );
//       };
//     | Cat(ls, annot) =>
//       let go_cat = (~piece_step=?, new_ds) => {
//         let (ns, _) =
//           List.fold_left(
//             (((txt1, ds1), origin), l) => {
//               let (txt2, ds2) =
//                 go(~piece_step?, ~indent, ~origin, dpaths, l);
//               ((txt1 @ txt2, ds1 @ ds2), origin + Layout.length(l));
//             },
//             (([], new_ds), origin),
//             ls,
//           );
//         ns;
//       };
//       switch (annot) {
//       | None => go_cat(~piece_step?, [])
//       | SelectionRange(_) =>
//         go_cat(~piece_step?, decs)
//       | Piece({color, shape, step}) =>
//         let new_ds =
//           DecPaths.current_piece(
//             ~show_neighbor_tiles,
//             ~measurement={origin, length: Layout.length(l)},
//             step,
//             color,
//             shape,
//             l,
//             dpaths,
//           )
//           |> List.map(Dec.view(~font_metrics));
//         //(~piece_step=step, ~indent, ~origin, dpaths, l);
//         //(txt, new_ds @ ds);
//         go_cat(~piece_step=step, new_ds);
//       | Child({step, sort: (_, s_in)}) =>
//         let piece_step =
//           piece_step
//           |> OptUtil.get_or_fail("expected to encounter piece before child");
//         let dpaths = DecPaths.take_two_step((piece_step, step), dpaths);
//         let new_ds =
//           DecPaths.current_bidelimited(
//             ~show_neighbor_tiles,
//             ~origin,
//             ~sort=s_in,
//             l,
//             dpaths,
//           )
//           |> List.map(Dec.view(~font_metrics));
//         // let (txt, ds) = go(~indent, ~origin, dpaths, l);
//         //(txt, new_ds @ ds);
//         go_cat(~piece_step, new_ds);
//       // | TargetBounds({sort, mode, strict_bounds}) =>
//       //   let len = len();
//       //   add_decorations([
//       //     TargetBoundsDec.view(
//       //       ~font_metrics,
//       //       ~origin=start,
//       //       ~len,
//       //       strict_bounds,
//       //       sort,
//       //       mode,
//       //     ),
//       //   ]);
//       };
//     /*
//      let (txt1, ds1) = go(~piece_step?, ~indent, ~origin, dpaths, l1);
//      let (txt2, ds2) =
//        go(
//          ~piece_step?,
//          ~indent,
//          ~origin=origin + Layout.length(l1),
//          dpaths,
//          l2,
//        );
//      (txt1 @ txt2, ds1 @ ds2);*/
//     //| Annot(annot, l) =>
//     };
//   };

//   let root_ds =
//     DecPaths.current_bidelimited(
//       ~show_neighbor_tiles,
//       ~origin=0,
//       ~sort=Exp,
//       l,
//       dpaths,
//     )
//     |> List.map(Dec.view(~font_metrics));
//   let (text, decorations) = go(dpaths, l);

//   let with_id =
//     fun
//     | None => []
//     | Some(id) => [Attr.id(id)];
//   let filler =
//     filler == 0
//       ? []
//       : [
//         Node.span(
//           [Attr.classes(["filler"])],
//           [
//             Node.text(
//               String.concat("", List.init(filler, _ => Unicode.nbsp)),
//             ),
//           ],
//         ),
//       ];
//   let text =
//     Node.span(
//       [Attr.classes(["code-text"]), ...with_id(text_id)],
//       text @ filler,
//     );
//   Node.div(
//     [Attr.classes(["code"]), ...with_id(id)],
//     [text, ...root_ds @ decorations],
//   );
// };
