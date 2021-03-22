open Virtual_dom.Vdom;
open Util;
open Core;

let tessera_children =
  Layout.measured_fold'(
    ~text=(_, _) => [],
    ~cat=_ => (@),
    ~annot=
      (_k, {start, len}, annot, _l) =>
        switch (annot) {
        | ClosedChild => [(start, len)]
        | _ => []
        },
  );

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
let tile_holes =
  Layout.measured_fold(
    ~text=(_, _) => [],
    ~cat=_ => (@),
    ~annot=
      ({start, _}, annot, holes) =>
        switch (annot) {
        | EmptyHole => [start, ...holes]
        | _ => holes
        },
  );

let rec view_of_layout =
        (~id=?, ~text_id=?, ~font_metrics: FontMetrics.t, l: Layout.t) => {
  let with_cls = cls => Node.span([Attr.classes([cls])]);
  let (text, decorations) =
    l
    |> Layout.measured_fold'(
         ~text=(_, s) => ([Node.text(s)], []),
         ~cat=(_, (txt1, ds1), (txt2, ds2)) => (txt1 @ txt2, ds1 @ ds2),
         ~annot=
           (k, {start, len}, annot, l) => {
             let (txt, ds) = k(l);
             let d_container =
               Decoration.container(
                 ~font_metrics,
                 ~origin=start,
                 ~length=len,
               );
             switch (annot) {
             | Delim => ([with_cls("delim", txt)], ds)
             | UniChild(sort, side) =>
               let d =
                 d_container(
                   ~cls="uni-child",
                   Decoration.UniChild.view({sort, side, len}),
                 );
               (txt, [d, ...ds]);
             | Tessera(shape, style) =>
               let closed_children = tessera_children(l);
               let d =
                 d_container(
                   ~cls="tessera",
                   Decoration.Tessera.view({
                     shape,
                     style,
                     closed_children,
                     len,
                   }),
                 );
               (txt, [d, ...ds]);
             | Tile(shape, style) =>
               let empty_holes = tile_holes(l);
               let (open_children, closed_children) = tile_children(l);
               let d =
                 d_container(
                   ~cls="tile",
                   Decoration.Tile.view(
                     ~font_metrics,
                     {
                       shape,
                       style,
                       len,
                       open_children,
                       closed_children,
                       empty_holes,
                     },
                   ),
                 );
               (txt, [d, ...ds]);
             | EmptyHole =>
               let d =
                 d_container(
                   ~cls="empty-hole",
                   Decoration.EmptyHole.view(~font_metrics, ~inset=None, ()),
                 );
               (txt, [d, ...ds]);
             | ErrHole(expanded) =>
               let d =
                 d_container(
                   ~cls="err-hole",
                   Decoration.ErrHole.view({expanded, len}),
                 );
               (txt, [d, ...ds]);
             | Selection(style) =>
               let d =
                 Decoration.Selection.view(~font_metrics, style, start, len);
               (txt, [d, ...ds]);
             | Grout(Some(caret)) =>
               let d =
                 Decoration.Caret.view(
                   ~font_metrics,
                   ~view_of_layout=
                     view_of_layout(~id=?None, ~text_id=?None, ~font_metrics),
                   start,
                   caret,
                 );
               (txt, [d, ...ds]);
             | Grout(None)
             | OpenChild
             | ClosedChild => (txt, ds)
             };
           },
       );
  let with_id =
    fun
    | None => []
    | Some(id) => [Attr.id(id)];
  let text =
    Node.span([Attr.classes(["code-text"]), ...with_id(text_id)], text);
  Node.div(
    [Attr.classes(["code"]), ...with_id(id)],
    [text, ...decorations],
  );
};
