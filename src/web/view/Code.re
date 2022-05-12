open Virtual_dom.Vdom;
open Node;
open Core;
open Util;

let span_c = cls => span([Attr.class_(cls)]);

let rec text_views = (l: Layout.t): list(Node.t) => {
  switch (l) {
  | Atom(token, ann) =>
    let ns = [text(Layout.string_of_token(token))];
    switch (ann) {
    | None
    | Ap
    | Space(_) => ns
    | EmptyHole(_) => [span_c("empty-hole", ns)]
    | Delim => [span_c("delim", ns)]
    | Shard => [span_c("extra-bold-delim", ns)]
    };
  | Cat(ls, _) => List.fold_left((ts, l) => ts @ text_views(l), [], ls)
  };
};

let range_profile = (ms: list(Layout.measured), i, j): Layout.measurement =>
  if (j == List.length(ms) && i < j) {
    let last = List.nth(ms, List.length(ms) - 1);
    let origin = List.nth(ms, i).measurement.origin;
    let length = last.measurement.origin + last.measurement.length - origin;
    {origin, length};
  } else if (j == List.length(ms) && i == j) {
    let last = List.nth(ms, List.length(ms) - 1);
    let origin = last.measurement.origin + last.measurement.length;
    let length = 0;
    {origin, length};
  } else {
    assert(0 <= i && i <= j && j < List.length(ms));
    let origin = List.nth(ms, i).measurement.origin;
    let length = List.nth(ms, j).measurement.origin - origin;
    {origin, length};
  };

let sel_piece_profile =
    (
      style: SelemStyle.t,
      mold: Mold.t,
      measurement: Layout.measurement,
      ms: list(Layout.measured),
    )
    : SelemDec.Profile.t => {
  let open_children =
    Layout.get_open_children(mold, ms)
    |> List.map((c: Layout.measured) => c.measurement)
    |> Layout.relativize_measurements(measurement.origin);
  let closed_children =
    Layout.get_closed_children(mold, ms)
    |> List.map((c: Layout.measured) => c.measurement)
    |> Layout.relativize_measurements(measurement.origin);
  {
    color: Color.of_sort(mold.sorts.out),
    shape: Layout.piece_shape_of_mold(mold),
    measurement,
    style,
    open_children,
    closed_children,
  };
};

let backpack_sel_view = ({focus: _, content}: Selection.t): t => {
  // TODO(andrew): Maybe use sort at caret instead of root
  let l = Layout.of_segment(Sort.root, content);
  let text_view = text_views(l);
  div([Attr.classes(["code-text", "backpack-selection"])], text_view);
};

let selection_length = (s: Selection.t): int =>
  // TODO(andrew): Maybe use sort at caret instead of root
  Layout.to_measured(Layout.of_segment(Sort.root, s.content)).measurement.
    length;

let genie_profile =
    (backpack: Backpack.t, origin: int): RestructuringGenieDec.Profile.t => {
  length: backpack |> List.map(selection_length) |> List.fold_left(max, 2),
  origin,
};

let backpack_view =
    (~font_metrics: FontMetrics.t, ~origin, backpack: Backpack.t): Node.t => {
  let style =
    Printf.sprintf(
      "position: absolute; left: %fpx; bottom: %fpx;",
      (Float.of_int(origin) -. 0.0) *. font_metrics.col_width,
      2. *. font_metrics.row_height,
    );
  let selections_view =
    div(
      [Attr.create("style", style), Attr.classes(["backpack"])],
      List.map(backpack_sel_view, List.rev(backpack)),
    );
  let genie_profile = genie_profile(backpack, origin);
  let genie_view = RestructuringGenieDec.view(~font_metrics, genie_profile);
  div([Attr.classes(["backpack"])], [selections_view, genie_view]);
};

let rec ind_hack = (l: Layout.t): list((Mold.t, Layout.token)) => {
  switch (l) {
  | Atom(_) => []
  | Cat(ls, Piece(_, mold, Indicated(token))) =>
    //TODO(andrew): hack
    [(mold, token)] @ List.fold_left((ts, l) => ts @ ind_hack(l), [], ls)
  | Cat(ls, _) => List.fold_left((ts, l) => ts @ ind_hack(l), [], ls)
  };
};

let caret_shape =
    (
      pd: list((Mold.t, Layout.token)),
      i: int,
      j: int,
      ms: list(Layout.measured),
      ~direction: Direction.t,
    )
    : CaretDec.caret_shape =>
  if (i == j) {
    //selection empty
    switch (pd) {
    /*| [({shape, _}, _)] =>
        switch (shape, direction) {
        | (Pre(_), _) => Left
        | (Post(_), _) => Right
        | (Op, Left) => Left
        | (Op, Right) => Right
        | (Bin(_), Left) => Right
        | (Bin(_), Right) => Left
        }
      | _ => Straight*/
    | [(_, {string, _})]
        when
          List.mem(
            string,
            ["+", "-", "*", "/", ",", "=", "in", "=>", "?", ":", ")", "]"],
          ) =>
      Right
    | [(_, {string, padding})]
        when padding == Bi && (string == " " || string == Unicode.nbsp) =>
      //grout case
      //TODO(andrew): improve logic
      Right
    | _ when j == List.length(ms) => Right // at end of program (hacky)
    | _ => Left
    };
  } else {
    switch (Util.ListUtil.split_sublist_opt(i, j, ms)) {
    | Some((_, [{layout, _}, ..._], _)) when direction == Left =>
      switch (Layout.get_shape(layout)) {
      | Some(Op | Pre(_)) => Left
      | _ => Right
      }
    | Some((_, sel, _)) when direction == Right =>
      let {layout, _}: Layout.measured = sel |> List.rev |> List.hd;
      switch (Layout.get_shape(layout)) {
      | Some(Op | Post(_)) => Right
      | _ => Left
      };
    | _ => Straight
    };
  };

let cat_decos =
    (
      pd: list((Mold.t, Layout.token)),
      ~font_metrics,
      ~backpack: Backpack.t,
      ~direction: Direction.t,
      ann: Layout.ann_cat,
      measurement: Layout.measurement,
      ms: list(Layout.measured),
    ) => {
  switch (ann) {
  | Segment(Range(i, j)) =>
    let profile = range_profile(ms, i, j);
    let origin =
      switch (direction) {
      | Left => profile.origin
      | Right => profile.origin + profile.length
      };
    // uncomment below for caret to be over by one for internal delims, infix ops
    /*let origin =
      switch (pd) {
      | [(_, {padding: Pre, _})]
      | [(_, {padding: Bi, _})] => origin + 1
      | _ => origin
      };*/
    let caret_shape = caret_shape(pd, i, j, ms, ~direction);
    [
      //TODO(andrew): restore
      //SelectedBoxDec.view(~font_metrics, profile),
      CaretDec.simple_view(~font_metrics, origin, caret_shape),
      backpack_view(~font_metrics, ~origin, backpack),
    ];
  | Piece(_p, mold, InsideFocalSegment(Selected)) =>
    let profile = sel_piece_profile(Filtered, mold, measurement, ms);
    [SelemDec.view(~font_metrics, profile)];
  | Piece(Grout, mold, Indicated(_s)) =>
    /*let measurement: Layout.measurement =
      switch (measurement.length) {
      | 1 => measurement
      | _ => {length: 1, origin: measurement.origin + 1}
      };*/
    let profile = sel_piece_profile(Root, mold, measurement, ms);
    [SelemDec.view(~font_metrics, profile)];
  //uncomment below to unfatten infix grout deco
  // TODO(andrew): hack
  | Piece(_, mold, Indicated(_s))
      when
        switch (pd) {
        | [(_, {string, _})] => List.mem(string, Token.ops)
        | _ => false
        } =>
    /*let measurement: Layout.measurement =
      switch (measurement.length) {
      | 1 => measurement
      | _ => {length: 1, origin: measurement.origin + 1}
      };*/
    let profile = sel_piece_profile(Root, mold, measurement, ms);
    [SelemDec.view(~font_metrics, profile)];
  //uncomment below to unfatten infix op deco
  // TODO(andrew): hack
  | Piece(_p, mold, Indicated(_s)) =>
    let profile = sel_piece_profile(Root, mold, measurement, ms);
    [SelemDec.view(~font_metrics, profile)];
  | _ => []
  };
};

let text_decos =
    (~font_metrics, ann: Layout.ann_atom, measurement: Layout.measurement) =>
  switch (ann) {
  | EmptyHole(mold) =>
    let measurement: Layout.measurement =
      switch (mold.shape) {
      | Bin(_) => {
          length: 1,
          origin:
            Layout.pad_segments ? measurement.origin : measurement.origin + 1,
        }
      | _ => measurement
      };
    [EmptyHoleDec.view(~font_metrics: FontMetrics.t, {measurement, mold})];
  | _ => []
  };

let rec deco_views =
        (
          ind_token,
          ~font_metrics: FontMetrics.t,
          ~backpack: Backpack.t,
          ~direction: Direction.t,
          layout: Layout.measured,
        )
        : list(t) =>
  switch (layout.layout) {
  | CatM(ms, ann) =>
    cat_decos(
      ind_token,
      ~font_metrics,
      ~backpack,
      ~direction,
      ann,
      layout.measurement,
      ms,
    )
    @ List.concat(
        List.map(
          deco_views(ind_token, ~font_metrics, ~backpack, ~direction),
          ms,
        ),
      )
  | AtomM(_s, ann) => text_decos(~font_metrics, ann, layout.measurement)
  };

let view =
    (
      ~font_metrics,
      ~just_failed as _: option(FailedInput.t)=None,
      ~show_neighbor_tiles as _: bool=false,
      ~zipper: Zipper.t,
    )
    : Node.t => {
  let layout = Layout.mk_zipper(zipper);
  let measured = Layout.to_measured(layout);
  let backpack = zipper.backpack;
  let direction = zipper.selection.focus;
  let ind_token = ind_hack(layout);
  div(
    [Attr.class_("code"), Attr.id("under-the-rail")],
    [
      span_c("code-text", text_views(layout)),
      ...deco_views(
           ind_token,
           ~font_metrics,
           ~backpack,
           ~direction,
           measured,
         ),
    ],
  );
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
//       | Shard => ([with_cls("extra-bold-delim", t)], [])
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
