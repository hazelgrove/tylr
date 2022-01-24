open Virtual_dom.Vdom;
open Util;
open Core;

let delete_actions =
  fun
  | (Subject.Pointing((prefix, suffix)), frame)
  | (Selecting(_, [], (prefix, suffix)), frame) => {
      let action = affix =>
        switch (affix) {
        | [Piece.Tile(tile), ..._] =>
          Tile.is_leaf(tile) ? Some(`Remove) : Some(`Restructure)
        | _ =>
          switch (frame) {
          | Frame.Exp(Root) => None
          | _ => Some(`Restructure)
          }
        };
      (action(prefix), action(suffix));
    }
  | (Selecting(_, selection, _), _) =>
    Selection.is_whole_any(selection)
      ? (Some(`Remove), Some(`Remove))
      : (Some(`Restructure), Some(`Restructure))
  | (Restructuring(_), _) => (Some(`Remove), Some(`Remove));

let rec view_of_layout =
        (
          ~id=?,
          ~text_id=?,
          ~font_metrics,
          ~zipper: option(Zipper.t)=?,
          ~filler=0,
          ~just_failed: option(FailedInput.t)=None,
          ~show_neighbor_tiles: bool=false,
          dpaths,
          l,
        )
        : Node.t => {
  let delete_actions = Option.map(delete_actions, zipper);
  let caret_mode =
    zipper
    |> Option.map(
         fun
         | (Subject.Pointing(_), _) => CaretMode.Pointing
         | (Selecting(side, selection, _), _) => Selecting(side, selection)
         | (Restructuring(((_, selection, rest) as backpack, rframe)), _) => {
             let at_restructurable_selection =
               switch (rframe) {
               | ([Selection(selection), ..._], _)
                   when
                     Option.is_some(Selection.is_restructurable(selection)) =>
                 true
               | (_, [Selection(selection), ..._])
                   when
                     Option.is_some(Selection.is_restructurable(selection)) =>
                 true
               | _ => false
               };
             let view_of_selection = (~with_box, selection) => {
               let l = Layout.mk_selection(~frame_color=Selected, selection);
               let len = List.length(selection);
               let dpaths =
                 DecPaths.mk(
                   ~caret=([], (0, len)),
                   ~selection_bars=[([], (0, len))],
                   ~selection_boxes=with_box ? [([], (0, len))] : [],
                   ~filtered_pieces=([], ListUtil.range(len)),
                   (),
                 );
               view_of_layout(~font_metrics, dpaths, l);
             };
             let selection = view_of_selection(~with_box=false, selection);
             let rest = List.map(view_of_selection(~with_box=true), rest);
             Restructuring({
               at_restructurable_selection,
               backpack,
               view: (selection, rest),
             });
           },
       );
  let with_cls = cls => Node.span([Attr.classes([cls])]);
  let rec go =
    (~piece_step=?, ~indent=0, ~origin=0, dpaths, l: Layout.t)
    : (list(Vdom.Node.t), list(Vdom.Node.t)) => {
    switch (l) {
    | Text(s) => ([Node.text(s)], [])
    | Cat(l1, l2) =>
      let (txt1, ds1) = go(~piece_step?, ~indent, ~origin, dpaths, l1);
      let (txt2, ds2) =
        go(
          ~piece_step?,
          ~indent,
          ~origin=origin + Layout.length(l1),
          dpaths,
          l2,
        );
      (txt1 @ txt2, ds1 @ ds2);
    | Annot(annot, l) =>
      let go' = () => go(~piece_step?, ~indent, ~origin, dpaths, l);
      let add_decorations = new_ds => {
        let (txt, ds) = go'();
        (txt, new_ds @ ds);
      };
      switch (annot) {
      | Space(step, color) =>
        DecPaths.current_space(
          ~delete_actions?,
          ~caret_mode?,
          ~just_failed,
          ~measurement={origin, length: 1},
          (step, color),
          dpaths,
        )
        |> List.map(Dec.view(~font_metrics))
        |> add_decorations

      | Ap =>
        let (txt, ds) = go'();
        (
          [with_cls("ap", txt)],
          [ApDec.view(~font_metrics, {origin, length: 1}), ...ds],
        );
      | ExtraBoldDelim =>
        let (txt, ds) = go'();
        ([with_cls("extra-bold-delim", txt)], ds);
      | Delim =>
        let (txt, ds) = go'();
        ([with_cls("delim", txt)], ds);
      | EmptyHole(color, tip) =>
        add_decorations([
          EmptyHoleDec.view(
            ~font_metrics: FontMetrics.t,
            {
              measurement: {
                origin,
                length: 1,
              },
              color,
              tip,
            },
          ),
        ])
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
        let (txt, ds) = go(~piece_step=step, ~indent, ~origin, dpaths, l);
        (txt, new_ds @ ds);

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
        let (txt, ds) = go(~indent, ~origin, dpaths, l);
        (txt, new_ds @ ds);
      };
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
