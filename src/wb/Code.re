open Virtual_dom.Vdom;
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
        ) => {
  let with_cls = cls => Node.span([Attr.classes([cls])]);
  let d_container = Decoration.container(~font_metrics);
  let rec go = (~tile_step=?, ~indent=0, ~start=0, dpaths, l: Layout.t) => {
    switch (l) {
    | Text(s) => ([Node.text(s)], [], 0)
    | Cat(l1, l2) =>
      let (txt1, ds1, n1) = go(~tile_step?, ~indent, ~start, dpaths, l1);
      let (txt2, ds2, n2) =
        go(
          ~tile_step?,
          ~indent,
          ~start=start + Layout.length(l1),
          dpaths,
          l2,
        );
      (txt1 @ txt2, ds1 @ ds2, n1 + n2);
    | Annot(annot, l) =>
      let d_container = d_container(~origin=start);
      let len = () => Layout.length(l);
      let go' = () => go(~tile_step?, ~indent, ~start, dpaths, l);
      let add_decorations = new_ds => {
        let (txt, ds, n) = go'();
        (txt, new_ds @ ds, n);
      };
      switch (annot) {
      | Step(step) =>
        switch (tile_step) {
        | None => go(~tile_step=step, ~indent, ~start, dpaths, l)
        | Some(tile_step) =>
          let dpaths =
            DecorationPaths.take_two_step((tile_step, step), dpaths);
          go(~indent, ~start, dpaths, l);
        }
      | Space(caret_step, color) =>
        let length = len();
        let (current_ds, current_filler) =
          DecorationPaths.current(caret_step, dpaths)
          |> List.filter_map(
               fun
               | DecorationShape.Sibling =>
                 Some((
                   [
                     d_container(
                       ~length,
                       ~cls="sibling",
                       Decoration.CaretPosition.view(
                         ~font_metrics,
                         ~style=`Sibling,
                         color,
                       ),
                     ),
                   ],
                   0,
                 ))
               | Anchor => {
                   Some((
                     [
                       d_container(
                         ~length,
                         ~cls="anchor",
                         Decoration.CaretPosition.view(
                           ~font_metrics,
                           ~style=`Anchor,
                           color,
                         ),
                       ),
                     ],
                     0,
                   ));
                 }

               // TODO clean up cousin terminology
               | OuterCousin => None
               //    (
               //      d_container(
               //        ~length,
               //        ~cls="outer-cousin",
               //        Decoration.CaretPosition.view(
               //          ~font_metrics,
               //          ~style=`OuterCousin,
               //          color,
               //        ),
               //      ),
               //      0,
               //    )
               | InnerCousin => None
               //   (
               //    d_container(
               //      ~length,
               //      ~cls="inner-cousin",
               //      Decoration.CaretPosition.view(
               //        ~font_metrics,
               //        ~style=`InnerCousin,
               //        color,
               //      ),
               //    ),
               //    0,
               //  )
               | Caret(mode) => {
                   let caret =
                     Decoration.Caret.view(
                       ~font_metrics,
                       ~view_of_layout=
                         view_of_layout(
                           ~id=?None,
                           ~text_id=?None,
                           ~subject=?None,
                         ),
                       ~color,
                       start,
                       mode,
                     );
                   switch (mode) {
                   | Pointing
                   | Selecting => Some(([caret], 0))
                   | Restructuring(selection) =>
                     let selection_len =
                       Layout.length(
                         Layout.mk_selection(Selected, selection),
                       )
                       - 1;
                     Some((
                       [
                         d_container(
                           ~length=selection_len,
                           ~cls="restructuring-genie",
                           Decoration.RestructuringGenie.view(
                             ~length=selection_len,
                           ),
                         ),
                         caret,
                       ],
                       selection_len,
                     ));
                   };
                 },
             )
          |> List.split
          |> Util.PairUtil.map_fst(List.flatten);
        let bare =
          switch (subject) {
          | None => []
          | Some(Restructuring(selection, _))
              when !Selection.is_whole_any(selection) =>
            []
          | _ => [
              d_container(
                ~length,
                ~cls="outer-cousin",
                Decoration.CaretPosition.view(
                  ~font_metrics,
                  ~style=`OuterCousin,
                  color,
                ),
              ),
            ]
          };
        let (txt, ds, n) = go'();
        (
          txt,
          bare @ current_ds @ ds,
          List.fold_left((+), 0, current_filler) + n,
        );
      | Delim =>
        let (txt, ds, n) = go'();
        ([with_cls("delim", txt)], ds, n);
      | EmptyHole(color, tip) =>
        add_decorations([
          d_container(
            ~length=len(),
            ~cls="empty-hole",
            Decoration.EmptyHole.view(~color, ~inset=None, ~tip, ()),
          ),
        ])
      | UniChild(sort, side) =>
        let len = len();
        add_decorations([
          d_container(
            ~length=len,
            ~cls="uni-child",
            Decoration.UniChild.view({sort, side, len}),
          ),
        ]);
      | Selected(sort_l, sort_r) =>
        let len = len();
        add_decorations([
          Decoration.SelectedBox.view(~font_metrics, ~start, ~len),
          d_container(
            ~length=len,
            ~cls="selected-bar",
            Decoration.SelectedBar.view(
              ~font_metrics,
              ~len,
              (sort_l, sort_r),
            ),
          ),
        ]);
      | Rail(style) =>
        let len = len();
        add_decorations([
          d_container(
            ~length=len,
            ~cls="rail",
            Decoration.Rail.view(~len, style),
          ),
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
            Decoration.Selem.view(
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
          Decoration.TargetBounds.view(
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

  let (text, decorations, filler) = go(dpaths, l);
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

// let rec view_of_layout =
//         (
//           ~id=?,
//           ~text_id=?,
//           ~font_metrics: FontMetrics.t,
//           dpaths: DecorationPaths.t,
//           l: Layout.t,
//         )
//         : Node.t => {
//   let with_cls = cls => Node.span([Attr.classes([cls])]);
//   let (text, decorations, filler) =
//     l
//     |> Layout.measured_fold'(
//          ~text=(_, s) => ([Node.text(s)], [], 0),
//          ~cat=
//            (_, (txt1, ds1, n1), (txt2, ds2, n2)) =>
//              (txt1 @ txt2, ds1 @ ds2, n1 + n2),
//          ~annot=
//            (k, {start, len}, annot, l) => {
//              let (txt, ds, filler) = k(l);
//              let d_container =
//                Decoration.container(
//                  ~font_metrics,
//                  ~origin=start,
//                  ~length=len,
//                );
//              let add_decoration = d => (txt, [d, ...ds], filler);
//              switch (annot) {
//              | Delim => ([with_cls("delim", txt)], ds, filler)
//              | UniChild(sort, side) =>
//                add_decoration(
//                  d_container(
//                    ~cls="uni-child",
//                    Decoration.UniChild.view({sort, side, len}),
//                  ),
//                )
//              | Selem(sort, shape, style) =>
//                let empty_holes = selem_holes(l);
//                let (open_children, closed_children) = tile_children(l);
//                let d =
//                  d_container(
//                    ~cls="tile",
//                    Decoration.Selem.view(
//                      ~font_metrics,
//                      ~start,
//                      {
//                        sort,
//                        shape,
//                        style,
//                        len,
//                        open_children,
//                        closed_children,
//                        empty_holes,
//                      },
//                    ),
//                  );
//                add_decoration(d);
//              | EmptyHole(sort) =>
//                let d =
//                  d_container(
//                    ~cls="empty-hole",
//                    Decoration.EmptyHole.view(
//                      ~font_metrics,
//                      ~sort,
//                      ~inset=None,
//                      (),
//                    ),
//                  );
//                add_decoration(d);
//              | Selected =>
//                let d = Decoration.SelectedBox.view(~font_metrics, start, len);
//                add_decoration(d);
//              | Space(Some((sort, caret))) =>
//                let d =
//                  Decoration.Caret.view(
//                    ~font_metrics,
//                    ~view_of_layout=view_of_layout(~id=?None, ~text_id=?None),
//                    ~sort,
//                    start,
//                    caret,
//                  );
//                let filler =
//                  switch (caret) {
//                  | Pointing
//                  | Selecting => filler
//                  | Restructuring(selection) =>
//                    let len = s => Layout.length(s) - 1;
//                    filler + len(selection);
//                  };
//                (txt, [d, ...ds], filler);
//              | Space(None)
//              | OpenChild
//              | ClosedChild => (txt, ds, filler)
//              };
//            },
//        );
//   let with_id =
//     fun
//     | None => []
//     | Some(id) => [Attr.id(id)];
//   let filler =
//     Node.span(
//       [Attr.classes(["filler"])],
//       [Node.text(String.concat("", List.init(filler, _ => Unicode.nbsp)))],
//     );
//   let text =
//     Node.span(
//       [Attr.classes(["code-text"]), ...with_id(text_id)],
//       text @ [filler],
//     );
//   Node.div(
//     [Attr.classes(["code"]), ...with_id(id)],
//     [text, ...decorations],
//   );
// };
