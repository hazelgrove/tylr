open Virtual_dom.Vdom;

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
        | EmptyHole(sort) => [(start, sort), ...holes]
        | _ => holes
        },
  );

let rec view_of_layout =
        (~id=?, ~text_id=?, ~font_metrics: FontMetrics.t, l: Layout.t): Node.t => {
  let with_cls = cls => Node.span([Attr.classes([cls])]);
  let (text, decorations, filler) =
    l
    |> Layout.measured_fold'(
         ~text=(_, s) => ([Node.text(s)], [], 0),
         ~cat=
           (_, (txt1, ds1, n1), (txt2, ds2, n2)) =>
             (txt1 @ txt2, ds1 @ ds2, n1 + n2),
         ~annot=
           (k, {start, len}, annot, l) => {
             let (txt, ds, filler) = k(l);
             let d_container =
               Decoration.container(
                 ~font_metrics,
                 ~origin=start,
                 ~length=len,
               );
             let add_decoration = d => (txt, [d, ...ds], filler);
             switch (annot) {
             | Delim => ([with_cls("delim", txt)], ds, filler)
             | UniChild(sort, side) =>
               add_decoration(
                 d_container(
                   ~cls="uni-child",
                   Decoration.UniChild.view({sort, side, len}),
                 ),
               )
             | Selem(sort, shape, style) =>
               let empty_holes = selem_holes(l);
               let (open_children, closed_children) = tile_children(l);
               let d =
                 d_container(
                   ~cls="tile",
                   Decoration.Selem.view(
                     ~font_metrics,
                     ~start,
                     {
                       sort,
                       shape,
                       style,
                       len,
                       open_children,
                       closed_children,
                       empty_holes,
                     },
                   ),
                 );
               add_decoration(d);
             | EmptyHole(sort) =>
               let d =
                 d_container(
                   ~cls="empty-hole",
                   Decoration.EmptyHole.view(
                     ~font_metrics,
                     ~sort,
                     ~inset=None,
                     (),
                   ),
                 );
               add_decoration(d);
             //  | Selected =>
             //    let d =
             //      Decoration.Selection.view(~font_metrics, style, start, len);
             //    add_decoration(d);
             | Space(Some(caret)) =>
               let d =
                 Decoration.Caret.view(
                   ~font_metrics,
                   ~view_of_layout=view_of_layout(~id=?None, ~text_id=?None),
                   start,
                   caret,
                 );
               let filler =
                 switch (caret) {
                 | Pointing
                 | Selecting => filler
                 | Restructuring(selection) =>
                   let len = s => Layout.length(s) - 1;
                   filler + len(selection);
                 };
               (txt, [d, ...ds], filler);
             | Space(None)
             | OpenChild
             | ClosedChild => (txt, ds, filler)
             };
           },
       );
  let with_id =
    fun
    | None => []
    | Some(id) => [Attr.id(id)];
  let filler =
    Node.span(
      [Attr.classes(["filler"])],
      [Node.text(String.concat("", List.init(filler, _ => Unicode.nbsp)))],
    );
  let text =
    Node.span(
      [Attr.classes(["code-text"]), ...with_id(text_id)],
      text @ [filler],
    );
  Node.div(
    [Attr.classes(["code"]), ...with_id(id)],
    [text, ...decorations],
  );
};
