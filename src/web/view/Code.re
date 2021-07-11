open Virtual_dom.Vdom;
open Util;
open Core;

let rec view_of_layout =
        (
          ~id=?,
          ~text_id=?,
          ~font_metrics,
          ~subject: option(Subject.t)=?,
          ~filler=0,
          ~just_failed: option(Action.t)=None,
          ~show_neighbor_tiles: bool=false,
          dpaths,
          l,
        )
        : Node.t => {
  let caret_mode =
    subject
    |> Option.map(
         fun
         | Subject.Pointing(_) => CaretMode.Pointing
         | Selecting(_) => Selecting
         | Restructuring(selection, _) => {
             let l = Layout.mk_selection(Selected, selection);
             let dpaths =
               DecPaths.mk(~caret=([], (0, List.length(selection))), ());
             Restructuring({
               selection,
               view: view_of_layout(~font_metrics, dpaths, l),
             });
           },
       );
  let with_cls = cls => Node.span([Attr.classes([cls])]);
  let rec go = (~selem_step=?, ~indent=0, ~origin=0, dpaths, l: Layout.t) => {
    switch (l) {
    | Text(s) => ([Node.text(s)], [])
    | Cat(l1, l2) =>
      let (txt1, ds1) = go(~selem_step?, ~indent, ~origin, dpaths, l1);
      let (txt2, ds2) =
        go(
          ~selem_step?,
          ~indent,
          ~origin=origin + Layout.length(l1),
          dpaths,
          l2,
        );
      (txt1 @ txt2, ds1 @ ds2);
    | Annot(annot, l) =>
      let go' = () => go(~selem_step?, ~indent, ~origin, dpaths, l);
      let add_decorations = new_ds => {
        let (txt, ds) = go'();
        (txt, new_ds @ ds);
      };
      switch (annot) {
      | Space(step, color) =>
        DecPaths.current_space(
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
      | Selem({color, shape, step}) =>
        let new_ds =
          DecPaths.current_selem(
            ~show_neighbor_tiles,
            ~measurement={origin, length: Layout.length(l)},
            step,
            color,
            shape,
            l,
            dpaths,
          )
          |> List.map(Dec.view(~font_metrics));
        let (txt, ds) = go(~selem_step=step, ~indent, ~origin, dpaths, l);
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
        let selem_step =
          selem_step
          |> OptUtil.get_or_fail("expected to encounter selem before child");
        let dpaths = DecPaths.take_two_step((selem_step, step), dpaths);
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
