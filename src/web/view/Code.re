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
        (
          ~id=?,
          ~text_id=?,
          ~font_metrics: FontMetrics.t,
          ~transparent=false,
          ~show_type_info=false,
          l: Layout.t,
        ) => {
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
             | Tessera(shape, style) =>
               let closed_children = tessera_children(l);
               let d =
                 d_container(
                   ~cls="tessera",
                   ~container_clss=transparent ? ["transparent"] : [],
                   Decoration.Tessera.view({
                     shape,
                     style,
                     closed_children,
                     len,
                   }),
                 );
               add_decoration(d);
             | Tile(shape, style) =>
               let empty_holes = tile_holes(l);
               let (open_children, closed_children) = tile_children(l);
               let d =
                 d_container(
                   ~cls="tile",
                   ~container_clss=transparent ? ["transparent"] : [],
                   Decoration.Tile.view(
                     ~font_metrics,
                     ~start,
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
               add_decoration(d);
             | EmptyHole =>
               let d =
                 d_container(
                   ~cls="empty-hole",
                   ~container_clss=transparent ? ["transparent"] : [],
                   Decoration.EmptyHole.view(~font_metrics, ~inset=None, ()),
                 );
               add_decoration(d);
             | ErrHole(expanded) =>
               let d =
                 d_container(
                   ~cls="err-hole",
                   Decoration.ErrHole.view({expanded, len}),
                 );
               add_decoration(d);
             | Selection(style) =>
               let d =
                 Decoration.Selection.view(~font_metrics, style, start, len);
               add_decoration(d);
             | Grout(Some(caret)) =>
               let d =
                 Decoration.Caret.view(
                   ~font_metrics,
                   ~view_of_layout=
                     view_of_layout(
                       ~id=?None,
                       ~text_id=?None,
                       ~show_type_info=false,
                       ~font_metrics,
                     ),
                   ~show_type_info,
                   start,
                   caret,
                 );
               let filler =
                 switch (caret) {
                 | Pointing(_)
                 | Selecting => filler
                 | Restructuring(selection, (prefix, suffix)) =>
                   let len = s => Layout.length(s) - 1;
                   let len_affix = List.fold_left((l, s) => l + len(s), 0);
                   filler
                   + len(selection)
                   + len_affix(prefix)
                   + len_affix(suffix);
                 };
               (txt, [d, ...ds], filler);
             | Grout(None)
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
      [
        Attr.classes(["code-text", ...transparent ? ["transparent"] : []]),
        ...with_id(text_id),
      ],
      text @ [filler],
    );
  Node.div(
    [Attr.classes(["code"]), ...with_id(id)],
    [text, ...decorations],
  );
};
