open Virtual_dom.Vdom;
open Util;
open Cor;

let rec view_of_layout =
        (
          ~id=?,
          ~text_id=?,
          ~font_metrics,
          ~subject: option(Subject.t)=?,
          dpaths,
          l,
        )
        : Node.t => {
  let (caret_mode, filler) =
    subject
    |> Option.map(
         fun
         | Subject.Pointing(_) => (CaretMode.Pointing, 0)
         | Selecting(_) => (Selecting, 0)
         | Restructuring(selection, _) => {
             let l = Layout.mk_selection(Selected, selection);
             let dpaths =
               DecPaths.mk(~caret=([], (0, List.length(selection))), ());
             (
               Restructuring({
                 selection,
                 view: view_of_layout(~font_metrics, dpaths, l),
               }),
               // TODO unify with length in restructuring genie
               Layout.length(l) - 1,
             );
           },
       )
    |> OptUtil.unzip;
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
      // | Step(step) =>
      //   switch (tile_step) {
      //   | None => go(~tile_step=step, ~indent, ~start, dpaths, l)
      //   | Some(tile_step) =>
      //     let dpaths = DecPaths.take_two_step((tile_step, step), dpaths);
      //     go(~indent, ~start, dpaths, l);
      //   }
      | Space(step, color) =>
        DecPaths.current_space(
          ~caret_mode?,
          ~measurement={origin, length: 1},
          (step, color),
          dpaths,
        )
        |> List.map(Dec.view(~font_metrics))
        |> add_decorations

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
              inset: None,
            },
          ),
        ])
      // | UniChild(sort, side) =>
      //   let len = len();
      //   add_decorations([
      //     d_container(
      //       ~length=len,
      //       ~cls="uni-child",
      //       UniChildDec.view({sort, side, len}),
      //     ),
      //   ]);
      // | Selected(sort_l, sort_r) =>
      //   let len = len();
      //   add_decorations([
      //     SelectedBoxDec.view(~font_metrics, ~start, ~len),
      //     d_container(
      //       ~length=len,
      //       ~cls="selected-bar",
      //       SelectedBarDec.view(~font_metrics, ~len, (sort_l, sort_r)),
      //     ),
      //   ]);
      // | Rail(style) =>
      //   let len = len();
      //   add_decorations([
      //     d_container(~length=len, ~cls="rail", RailDec.view(~len, style)),
      //   ]);
      | Selem({color, shape, step}) =>
        let new_ds =
          DecPaths.current_selem(
            ~measurement={origin, length: 1},
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
        print_endline("-- 0 --");
        print_endline("origin = " ++ string_of_int(origin));
        print_endline(
          Sexplib.Sexp.to_string_hum(DecPaths.sexp_of_t(dpaths)),
        );
        print_endline(
          "s_in = " ++ Sexplib.Sexp.to_string(Sort.sexp_of_t(s_in)),
        );
        let dpaths = DecPaths.take_two_step((selem_step, step), dpaths);
        let new_ds =
          DecPaths.current_bidelimited(~origin, ~sort=s_in, l, dpaths)
          |> List.map(Dec.view(~font_metrics));
        let (txt, ds) = go(~indent, ~origin, dpaths, l);
        (txt, new_ds @ ds);
      };
    };
  };

  let root_ds =
    DecPaths.current_bidelimited(~origin=0, ~sort=Exp, l, dpaths)
    |> List.map(Dec.view(~font_metrics));
  let (text, decorations) = go(dpaths, l);

  let with_id =
    fun
    | None => []
    | Some(id) => [Attr.id(id)];
  let filler = {
    let n =
      switch (filler) {
      | None => 0
      | Some(n) => n
      };
    n == 0
      ? []
      : [
        Node.span(
          [Attr.classes(["filler"])],
          [Node.text(String.concat("", List.init(n, _ => Unicode.nbsp)))],
        ),
      ];
  };
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
