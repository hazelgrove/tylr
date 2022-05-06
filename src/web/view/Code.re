open Virtual_dom.Vdom;
open Util;
open Core;

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

// let rec view_of_layout =
let view_of_layout =
    (
      ~id=?,
      ~text_id=?,
      ~font_metrics,
      ~zipper as _: option(Zipper.t)=?,
      ~filler=0,
      ~just_failed as _: option(FailedInput.t)=None,
      ~show_neighbor_tiles: bool=false,
      dpaths,
      l,
    )
    : Node.t => {
  // let delete_actions = Option.map(delete_actions, zipper);
  // let caret_mode =
  //   zipper
  //   |> Option.map(
  //        fun
  //        | (Subject.Pointing(_), _) => CaretMode.Pointing
  //        | (Selecting(side, selection, _), _) => Selecting(side, selection)
  //        | (Restructuring(((_, selection, rest) as backpack, rframe)), _) => {
  //            let at_restructurable_selection =
  //              switch (rframe) {
  //              | ([Selection(selection), ..._], _)
  //                  when
  //                    Option.is_some(Selection.is_restructurable(selection)) =>
  //                true
  //              | (_, [Selection(selection), ..._])
  //                  when
  //                    Option.is_some(Selection.is_restructurable(selection)) =>
  //                true
  //              | _ => false
  //              };
  //            let view_of_selection = (~with_box, selection) => {
  //              let l = Layout.mk_selection(~frame_color=Selected, selection);
  //              let len = List.length(selection);
  //              let dpaths =
  //                DecPaths.mk(
  //                  ~caret=([], (0, len)),
  //                  ~selection_bars=[([], (0, len))],
  //                  ~selection_boxes=with_box ? [([], (0, len))] : [],
  //                  ~filtered_pieces=([], ListUtil.range(len)),
  //                  (),
  //                );
  //              view_of_layout(~font_metrics, dpaths, l);
  //            };
  //            let selection = view_of_selection(~with_box=false, selection);
  //            let rest = List.map(view_of_selection(~with_box=true), rest);
  //            Restructuring({
  //              at_restructurable_selection,
  //              backpack,
  //              view: (selection, rest),
  //            });
  //          },
  //      );
  let rec get_decs = (layout: Layout.measured) =>
    switch (layout) {
    | {layout: CatM(ms, SelectionRange(first, last)), _} =>
      let f = List.nth(ms, first).measurement.origin;
      let l =
        List.nth(ms, last).measurement.origin
        + List.nth(ms, last).measurement.length;
      Printf.printf("SELECTION RANGE: %d %d\n", f, l);
      [SelectedBoxDec.view(~font_metrics, {origin: f, length: l - f})];
    | {layout: CatM(ms, _), _} => List.concat(List.map(get_decs, ms))
    | _ => []
    };
  let decs = get_decs(Layout.to_measured(l));

  let with_cls = cls => Node.span([Attr.classes([cls])]);
  let rec go =
          (~piece_step=?, ~indent=0, ~origin=0, dpaths, l: Layout.t)
          : (list(Node.t), list(Node.t)) => {
    switch (l) {
    | Text(s, text_ann) =>
      let t = [Node.text(s)];
      switch (text_ann) {
      | None => (t, [])
      | Delim => ([with_cls("delim", t)], [])
      | DelimBold => ([with_cls("extra-bold-delim", t)], [])
      | Ap => (
          [with_cls("ap", t)],
          [ApDec.view(~font_metrics, {origin, length: 1})],
        )
      | Space(_step, _color) =>
        // DecPaths.current_space(
        //   ~delete_actions?,
        //   ~caret_mode?,
        //   ~just_failed,
        //   ~measurement={origin, length: 1},
        //   (step, color),
        //   dpaths,
        // )
        // |> List.map(Dec.view(~font_metrics))
        // |> add_decorations
        (t, [])
      | EmptyHole(color, _tip) =>
        let m: Layout.measurement = {origin, length: 1};
        (
          t,
          [
            EmptyHoleDec.view(
              ~font_metrics: FontMetrics.t,
              {measurement: m, color},
            ),
          ],
        );
      };
    | Cat(ls, annot) =>
      let go_cat = (~piece_step=?, new_ds) => {
        let (ns, _) =
          List.fold_left(
            (((txt1, ds1), origin), l) => {
              let (txt2, ds2) =
                go(~piece_step?, ~indent, ~origin, dpaths, l);
              ((txt1 @ txt2, ds1 @ ds2), origin + Layout.length(l));
            },
            (([], new_ds), origin),
            ls,
          );
        ns;
      };
      switch (annot) {
      | None => go_cat(~piece_step?, [])
      | SelectionRange(_) =>
        //TODO(andrew)
        go_cat(~piece_step?, decs)
      | Piece({color, shape, step}) =>
        let new_ds =
          DecPaths.current_piece(
            ~show_neighbor_tiles,
            ~measurement={origin, length: Layout.length(l)},
            step,
            color,
            shape,
            l,
            dpaths,
          )
          |> List.map(Dec.view(~font_metrics));
        //(~piece_step=step, ~indent, ~origin, dpaths, l);
        //(txt, new_ds @ ds);
        go_cat(~piece_step=step, new_ds);
      | Child({step, sort: (_, s_in)}) =>
        let piece_step =
          piece_step
          |> OptUtil.get_or_fail("expected to encounter piece before child");
        let dpaths = DecPaths.take_two_step((piece_step, step), dpaths);
        let new_ds =
          DecPaths.current_bidelimited(
            ~show_neighbor_tiles,
            ~origin,
            ~sort=s_in,
            l,
            dpaths,
          )
          |> List.map(Dec.view(~font_metrics));
        // let (txt, ds) = go(~indent, ~origin, dpaths, l);
        //(txt, new_ds @ ds);
        go_cat(~piece_step, new_ds);
      // | TargetBounds({sort, mode, strict_bounds}) =>
      //   let len = len();
      //   add_decorations([
      //     TargetBoundsDec.view(
      //       ~font_metrics,
      //       ~origin=start,
      //       ~len,
      //       strict_bounds,
      //       sort,
      //       mode,
      //     ),
      //   ]);
      };
    /*
     let (txt1, ds1) = go(~piece_step?, ~indent, ~origin, dpaths, l1);
     let (txt2, ds2) =
       go(
         ~piece_step?,
         ~indent,
         ~origin=origin + Layout.length(l1),
         dpaths,
         l2,
       );
     (txt1 @ txt2, ds1 @ ds2);*/
    //| Annot(annot, l) =>
    };
  };

  let root_ds =
    DecPaths.current_bidelimited(
      ~show_neighbor_tiles,
      ~origin=0,
      ~sort=Exp,
      l,
      dpaths,
    )
    |> List.map(Dec.view(~font_metrics));
  let (text, decorations) = go(dpaths, l);

  let with_id =
    fun
    | None => []
    | Some(id) => [Attr.id(id)];
  let filler =
    filler == 0
      ? []
      : [
        Node.span(
          [Attr.classes(["filler"])],
          [
            Node.text(
              String.concat("", List.init(filler, _ => Unicode.nbsp)),
            ),
          ],
        ),
      ];
  let text =
    Node.span(
      [Attr.classes(["code-text"]), ...with_id(text_id)],
      text @ filler,
    );
  Node.div(
    [Attr.classes(["code"]), ...with_id(id)],
    [text, ...root_ds @ decorations],
  );
};
