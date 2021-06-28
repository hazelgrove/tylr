open Virtual_dom.Vdom;
open Util;
open Cor;

let tile_children =
  Layout.measured_fold'(
    ~text=(_, _) => ([], []),
    ~cat=
      (_, (open1, closed1), (open2, closed2)) =>
        (open1 @ open2, closed1 @ closed2),
    ~annot=
      (_k, {start, len}, annot, _l) =>
        switch (annot) {
        | OpenChild => ([(start, len)], [])
        | ClosedChild => ([], [(start, len)])
        | _ => ([], [])
        },
  );
let selem_holes =
  Layout.measured_fold(
    ~text=(_, _) => [],
    ~cat=_ => (@),
    ~annot=
      ({start, _}, annot, holes) =>
        switch (annot) {
        | EmptyHole(sort, tip) => [(start, sort, tip), ...holes]
        | _ => holes
        },
  );

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
             let l =
               Layout.mk_selection(~style=Filtered, Selected, selection);
             let dpaths =
               DecPaths.{
                 caret: Some(([], (0, List.length(selection)))),
                 siblings: None,
               };
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
  let d_container = DecUtil.container(~font_metrics);
  let rec go = (~tile_step=?, ~indent=0, ~start=0, dpaths, l: Layout.t) => {
    switch (l) {
    | Text(s) => ([Node.text(s)], [])
    | Cat(l1, l2) =>
      let (txt1, ds1) = go(~tile_step?, ~indent, ~start, dpaths, l1);
      let (txt2, ds2) =
        go(
          ~tile_step?,
          ~indent,
          ~start=start + Layout.length(l1),
          dpaths,
          l2,
        );
      (txt1 @ txt2, ds1 @ ds2);
    | Annot(annot, l) =>
      let d_container = d_container(~origin=start);
      let len = () => Layout.length(l);
      let go' = () => go(~tile_step?, ~indent, ~start, dpaths, l);
      let add_decorations = new_ds => {
        let (txt, ds) = go'();
        (txt, new_ds @ ds);
      };
      switch (annot) {
      | Step(step) =>
        switch (tile_step) {
        | None => go(~tile_step=step, ~indent, ~start, dpaths, l)
        | Some(tile_step) =>
          let dpaths = DecPaths.take_two_step((tile_step, step), dpaths);
          go(~indent, ~start, dpaths, l);
        }
      | Space(step, color) =>
        DecPaths.current(~caret_mode?, ~origin=start, (step, color), dpaths)
        |> List.map(Dec.view(~font_metrics))
        |> add_decorations

      | Delim =>
        let (txt, ds) = go'();
        ([with_cls("delim", txt)], ds);
      | EmptyHole(color, tip) =>
        add_decorations([
          d_container(
            ~length=len(),
            ~cls="empty-hole",
            EmptyHoleDec.view(~color, ~inset=None, ~tip, ()),
          ),
        ])
      | UniChild(sort, side) =>
        let len = len();
        add_decorations([
          d_container(
            ~length=len,
            ~cls="uni-child",
            UniChildDec.view({sort, side, len}),
          ),
        ]);
      | Selected(sort_l, sort_r) =>
        let len = len();
        add_decorations([
          SelectedBoxDec.view(~font_metrics, ~start, ~len),
          d_container(
            ~length=len,
            ~cls="selected-bar",
            SelectedBarDec.view(~font_metrics, ~len, (sort_l, sort_r)),
          ),
        ]);
      | Rail(style) =>
        let len = len();
        add_decorations([
          d_container(~length=len, ~cls="rail", RailDec.view(~len, style)),
        ]);
      | Selem({color, shape, style}) =>
        let empty_holes = selem_holes(l);
        let (open_children, closed_children) = tile_children(l);
        let len = len();
        let d =
          d_container(
            ~length=len,
            ~cls="tile",
            ~container_clss=[SelemStyle.to_string(style)],
            SelemDec.view(
              ~font_metrics,
              ~start,
              {
                color,
                shape,
                style,
                len,
                open_children,
                closed_children,
                empty_holes,
              },
            ),
          );
        add_decorations([d]);
      | TargetBounds({sort, mode, strict_bounds}) =>
        let len = len();
        add_decorations([
          TargetBoundsDec.view(
            ~font_metrics,
            ~origin=start,
            ~len,
            strict_bounds,
            sort,
            mode,
          ),
        ]);
      | OpenChild
      | ClosedChild => go'()
      };
    };
  };

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
    [text, ...decorations],
  );
};
