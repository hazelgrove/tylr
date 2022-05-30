open Virtual_dom.Vdom;
open Node;
open Core;
open Util;

let span_c = cls => span([Attr.class_(cls)]);

module Text = {
  let rec of_segment = (seg: Segment.t): list(Node.t) =>
    seg
    |> List.map(
         fun
         | Piece.Whitespace(w) => [Node.text(w.content)]
         | Grout(_) => [Node.text(Unicode.nbsp)]
         | Tile(t) => of_tile(t),
       )
    |> List.concat
  and of_tile = (t: Tile.t): list(Node.t) => {
    let span =
      List.length(t.label) == 1
        ? Node.span([])
        : span_c(Tile.is_complete(t) ? "delim" : "extra-bold-delim");
    Aba.mk(t.shards, t.children)
    |> Aba.join(
         i => [span([Node.text(List.nth(t.label, i))])],
         of_segment,
       )
    |> List.concat;
  };
};

// let rec text_views = (l: Layout.t): list(Node.t) => {
//   switch (l) {
//   | Atom(s, ann) =>
//     switch (ann) {
//     | None
//     | Whitespace
//     | Ap
//     | Space(_)
//     | EmptyHole(_) => [text(s)]
//     | Delim => [span_c("delim", [text(s)])]
//     | Shard => [span_c("extra-bold-delim", [text(s)])]
//     }
//   | Cat(ls, _) => List.fold_left((ts, l) => ts @ text_views(l), [], ls)
//   };
// };

// let range_profile = (ms: list(Layout.measured), i, j): Layout.measurement =>
//   if (j == List.length(ms) && i < j) {
//     let last = List.nth(ms, List.length(ms) - 1);
//     let origin = List.nth(ms, i).measurement.origin;
//     let length = last.measurement.origin + last.measurement.length - origin;
//     {origin, length};
//   } else if (j == List.length(ms) && i == j) {
//     let last = List.nth(ms, List.length(ms) - 1);
//     let origin = last.measurement.origin + last.measurement.length;
//     let length = 0;
//     {origin, length};
//   } else {
//     assert(0 <= i && i <= j && j < List.length(ms));
//     let origin = List.nth(ms, i).measurement.origin;
//     let length = List.nth(ms, j).measurement.origin - origin;
//     {origin, length};
//   };

// let sel_piece_profile =
//     (
//       style: SelemStyle.t,
//       mold: Mold.t,
//       measurement: Layout.measurement,
//       ms: list(Layout.measured),
//     )
//     : SelemDec.Profile.t => {
//   let open_children =
//     Layout.get_open_children(mold, ms)
//     |> List.map((c: Layout.measured) => c.measurement)
//     |> Layout.relativize_measurements(measurement.origin);
//   let closed_children =
//     Layout.get_closed_children(mold, ms)
//     |> List.map((c: Layout.measured) => c.measurement)
//     |> Layout.relativize_measurements(measurement.origin);
//   {
//     color: Color.of_sort(mold.out),
//     shape: Layout.piece_shape_of_mold(mold),
//     measurement,
//     style,
//     open_children,
//     closed_children,
//   };
// };

let backpack_sel_view = ({focus: _, content}: Selection.t): t => {
  // TODO(andrew): Maybe use sort at caret instead of root
  let text_view = Text.of_segment(content);
  div([Attr.classes(["code-text", "backpack-selection"])], text_view);
};

let selection_length = (sel: Selection.t): int => {
  let seg = sel.content;
  let (len, _) = Measured.of_segment(seg);
  len;
  // switch (ListUtil.hd_opt(seg), ListUtil.last_opt(seg)) {
  // | (None, _)
  // | (_, None) => 0
  // | (Some(first), Some(last)) =>
  //   let first = Measured.find_p(first, map);
  //   let last = Measured.find_p(last, map);
  //   last.origin + last.length - first.origin;
  // };
};

let genie_profile =
    (backpack: Backpack.t, origin: int): RestructuringGenieDec.Profile.t => {
  length: backpack |> List.map(selection_length) |> List.fold_left(max, 0),
  origin,
};

let backpack_view =
    (~font_metrics: FontMetrics.t, ~origin, backpack: Backpack.t): Node.t => {
  let style =
    Printf.sprintf(
      "position: absolute; left: %fpx; bottom: %fpx;",
      (Float.of_int(origin) -. 1.0) *. font_metrics.col_width,
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

// let cat_decos =
//     (
//       ~font_metrics,
//       ~zipper as {backpack, selection: {focus, _}, caret, _}: Zipper.t,
//       ann: Layout.ann_cat,
//       measurement: Layout.measurement,
//       ms,
//     ) => {
//   let caret_offset =
//     switch (caret) {
//     | Outer => 0
//     | Inner(n) => n + 1
//     };
//   //NOTE(andrew): below related to generic spacing
//   let sub_offset =
//     switch (caret) {
//     | Outer => (-0.5)
//     | Inner(_) => 0.0
//     };
//   switch (ann) {
//   | Segment(Range(i, j)) =>
//     let profile = range_profile(ms, i, j);
//     let origin =
//       switch (focus) {
//       | Left => profile.origin
//       | Right => profile.origin + profile.length
//       };
//     [
//       SelectedBoxDec.view(~font_metrics, profile),
//       //debug caret:
//       //CaretDec.simple_view(~font_metrics, ~sub_offset, origin, "#00f8"),
//       CaretDec.simple_view(
//         ~font_metrics,
//         ~sub_offset,
//         origin + caret_offset,
//         "#f008",
//       ),
//       backpack_view(~font_metrics, ~origin, backpack),
//     ];
//   | Piece(_p, mold, InsideFocalSegment(Selected)) =>
//     let profile = sel_piece_profile(Selected, mold, measurement, ms);
//     [SelemDec.view(~font_metrics, profile)];
//   | Piece(_p, mold, Indicated) =>
//     let profile = sel_piece_profile(Root, mold, measurement, ms);
//     [SelemDec.view(~font_metrics, profile)];
//   | _ => []
//   };
// };

// let text_decos = (~font_metrics, ann: Layout.ann_atom, measurement) =>
//   switch (ann) {
//   | EmptyHole(mold) => [
//       EmptyHoleDec.view(~font_metrics: FontMetrics.t, {measurement, mold}),
//     ]
//   | _ => []
//   };

// let rec deco_views =
//         (
//           ~font_metrics: FontMetrics.t,
//           ~zipper: Zipper.t,
//           layout: Layout.measured,
//         )
//         : list(t) => {
//   switch (layout.layout) {
//   | CatM(ms, ann) =>
//     cat_decos(~font_metrics, ~zipper, ann, layout.measurement, ms)
//     @ List.concat(List.map(deco_views(~font_metrics, ~zipper), ms))
//   | AtomM(_s, ann) => text_decos(~font_metrics, ann, layout.measurement)
//   };
// };

module Deco = (M: {
                 let font_metrics: FontMetrics.t;
                 let map: Measured.t;
               }) => {
  let font_metrics = M.font_metrics;

  let rec holes = (seg: Segment.t): list(Node.t) =>
    seg
    |> List.map(
         fun
         | Piece.Whitespace(_) => []
         | Tile(t) => t.children |> List.map(holes) |> List.concat
         | Grout(g) => {
             // TODO(d) fix sort
             let mold = Mold.of_grout(g, Exp);
             let measurement = Measured.find_g(g, M.map);
             [EmptyHoleDec.view(~font_metrics, {measurement, mold})];
           },
       )
    |> List.concat;

  let selection_profile = (z: Zipper.t): Layout.measurement => {
    let sel = z.selection.content;
    let origin =
      switch (z.relatives.siblings) {
      | (_, [p, ..._]) => Measured.find_p(p, M.map).origin
      | ([p, ..._], _) =>
        let m = Measured.find_p(p, M.map);
        m.origin + m.length + 1;
      | ([], []) =>
        let p =
          ListUtil.hd_opt(sel) |> OptUtil.get_or_raise(Segment.Empty_segment);
        Measured.find_p(p, M.map).origin;
      };
    let length =
      switch (ListUtil.hd_opt(sel), ListUtil.last_opt(sel)) {
      | (None, _)
      | (_, None) => 0
      | (Some(first), Some(last)) =>
        let first = Measured.find_p(first, M.map);
        let last = Measured.find_p(last, M.map);
        last.origin + last.length + 1 - first.origin;
      };
    {origin, length};
  };

  let caret = (z: Zipper.t): list(Node.t) => {
    let profile = selection_profile(z);
    let origin =
      switch (z.selection.focus) {
      | Left => profile.origin
      | Right => profile.origin + profile.length
      };
    //NOTE(andrew): below related to generic spacing
    let (caret_offset, sub_offset) =
      switch (z.caret) {
      | Outer => (0, (-0.5))
      | Inner(n) => (n + 1, 0.0)
      };
    [
      SelectedBoxDec.view(~font_metrics, profile),
      //debug caret:
      //CaretDec.simple_view(~font_metrics, ~sub_offset, origin, "#00f8"),
      CaretDec.simple_view(
        ~font_metrics,
        ~sub_offset,
        origin + caret_offset,
        "#f008",
      ),
      backpack_view(~font_metrics, ~origin, z.backpack),
    ];
  };

  let rec targets = (bp: Backpack.t, seg: Segment.t) => {
    let root_targets =
      ListUtil.splits(seg)
      |> List.map(((l, r)) => {
           let sibs = Segment.(incomplete_tiles(l), incomplete_tiles(r));
           switch (Backpack.pop(sibs, bp)) {
           | None
           | Some((true, _, _)) => []
           | Some(_) =>
             let measurement =
               switch (l, r) {
               | ([], []) => failwith("impossible")
               | (_, [p, ..._]) =>
                 let m = Measured.find_p(p, M.map);
                 Layout.{origin: m.origin - 1, length: 1};
               | ([p, ..._], _) =>
                 let m = Measured.find_p(p, M.map);
                 Layout.{origin: m.origin + m.length, length: 1};
               };
             let profile =
               CaretPosDec.Profile.{
                 style: `Sibling,
                 measurement,
                 color: Color.Exp,
                 just_failed: None,
               };
             [CaretPosDec.view(~font_metrics, profile)];
           };
         })
      |> List.concat;
    switch (root_targets) {
    | [_, ..._] => root_targets
    | [] =>
      seg
      |> List.filter_map(
           fun
           | Piece.Tile(t) => Some(t)
           | _ => None,
         )
      |> List.map((t: Tile.t) =>
           List.concat(List.map(targets(bp), t.children))
         )
      |> List.concat
    };
  };

  let all = (z: Zipper.t) => {
    let seg = Zipper.zip(z);
    List.concat([holes(seg), caret(z), targets(z.backpack, seg)]);
  };
};

let view =
    (
      ~font_metrics,
      ~just_failed as _: option(FailedInput.t)=None,
      ~show_neighbor_tiles as _: bool=false,
      ~zipper: Zipper.t,
    )
    : Node.t => {
  // let layout = Layout.mk_zipper(zipper);
  // let measured = Layout.to_measured(layout);
  let seg = Zipper.zip(zipper);
  module Deco =
    Deco({
      let font_metrics = font_metrics;
      let map = snd(Measured.of_segment(seg));
    });
  div(
    [Attr.class_("code"), Attr.id("under-the-rail")],
    [span_c("code-text", Text.of_segment(seg))]
    // [
    //   span_c("code-text", text_views(layout)),
    //   ...deco_views(~font_metrics, ~zipper, measured),
    // ]
    @ Deco.all(zipper),
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
