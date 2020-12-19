open Virtual_dom.Vdom;
open Util;
open Core;

let empty_holes = (~font_metrics: FontMetrics.t, e: HExp.t): list(Node.t) =>
  Measured.Exp.empty_holes(e)
  |> List.map(origin =>
       Decoration.container(
         ~font_metrics,
         ~origin,
         ~length=1,
         ~cls="empty-hole",
         Decoration.EmptyHole.view(~font_metrics, ~inset=None, ()),
       )
     );

let err_holes = (~font_metrics, focus: ZPath.t, e: HExp.t): list(Node.t) => {
  Measured.Exp.err_holes_z(focus, e)
  |> List.map(((origin, profile: Decoration.ErrHole.profile)) =>
       Decoration.container(
         ~font_metrics,
         ~origin,
         ~length=profile.len,
         ~cls="err-hole",
         Decoration.ErrHole.view(profile),
       )
     );
};

let view_of_tile =
    (~font_metrics, (offset, profile: Decoration.Tile.profile)) =>
  Decoration.container(
    ~font_metrics,
    ~length=profile.len,
    ~cls="tile",
    ~origin=offset,
    Decoration.Tile.view(~font_metrics, profile),
  );

let view_of_text = tokens =>
  Node.span([Attr.classes(["code-text"])], Text.space(tokens));

let view_of_normal = (~font_metrics, path, e) => {
  let text = view_of_text(Text.Exp.view(e));
  let empty_holes = empty_holes(~font_metrics, e);
  let err_holes = err_holes(~font_metrics, path, e);
  let current_term = {
    let (root_tile, open_children) = Measured.Exp.term_profile(path, e);
    let root_tile = view_of_tile(~font_metrics, root_tile);
    let open_children =
      open_children
      |> List.map(
           ((offset, Decoration.OpenChild.{side, len, _} as profile)) => {
           Decoration.container(
             ~font_metrics,
             ~length=len + 1,
             ~origin=
               switch (side) {
               | Left => offset
               | Right => offset - 1
               },
             ~cls="open-child",
             Decoration.OpenChild.view(profile),
           )
         });
    [root_tile, ...open_children];
  };
  let caret =
    Decoration.Caret.view(~font_metrics, Measured.Exp.offset(path, e), []);
  List.concat([empty_holes, err_holes, current_term, [text, caret]]);
};

let view_of_selecting = (~font_metrics: FontMetrics.t, selection, e) => {
  let ((l, r) as selection, caret_side) =
    ZPath.mk_ordered_selection(selection);
  let (offset_l, offset_r) = (
    Measured.Exp.offset(l, e),
    Measured.Exp.offset(r, e),
  );
  let text = view_of_text(Text.Exp.view(e));
  let empty_holes = empty_holes(~font_metrics, e);
  let tiles = {
    let (tiles, _) = Measured.Exp.selecting_tiles(selection, e);
    List.map(view_of_tile(~font_metrics), tiles);
  };
  let selection_box =
    Node.div(
      [
        Attr.classes(["selection-box"]),
        Attr.create(
          "style",
          Printf.sprintf(
            "left: %fpx; top: calc(%fpx + 2px); width: %fpx; height: %fpx;",
            (Float.of_int(offset_l) +. 0.5) *. font_metrics.col_width,
            (-0.15) *. font_metrics.row_height,
            font_metrics.col_width *. Float.of_int(offset_r - offset_l),
            font_metrics.row_height *. 1.3,
          ),
        ),
      ],
      [],
    );
  let caret =
    Decoration.Caret.view(
      ~font_metrics,
      caret_side == Left ? offset_l : offset_r,
      [],
    );
  List.concat([empty_holes, tiles, [text, selection_box, caret]]);
};

let view_of_restructuring =
    (~font_metrics: FontMetrics.t, selection, target, e) => {
  let (offset_l, offset_r, offset_t) = {
    let (l, r) = selection;
    TupleUtil.map3(p => Measured.Exp.offset(p, e), (l, r, target));
  };
  let ZList.{prefix, z: selected, suffix} =
    Text.Exp.view_of_selection(selection, e);
  let text = view_of_text(prefix @ selected @ suffix);
  let empty_holes = empty_holes(~font_metrics, e);
  let (selected_tiles, target_tiles) =
    Measured.Exp.restructuring_tiles(selection, target, e);
  let placeholder =
    Node.span(
      [
        Attr.classes(["placeholder"]),
        Attr.create(
          "style",
          Printf.sprintf(
            "top: 2px; left: %fpx; width: %fpx;",
            Float.of_int(offset_l) *. font_metrics.col_width,
            font_metrics.col_width *. Float.of_int(offset_r - offset_l),
          ),
        ),
      ],
      [
        Node.div(
          [
            Attr.classes(["placeholder-mask"]),
            Attr.create(
              "style",
              Printf.sprintf(
                "left: %fpx; top: %fpx; width: %fpx; height: %fpx;",
                0.5 *. font_metrics.col_width,
                (-0.15) *. font_metrics.row_height,
                font_metrics.col_width *. Float.of_int(offset_r - offset_l),
                font_metrics.row_height *. 1.3,
              ),
            ),
          ],
          [],
        ),
      ],
    );
  let (caret, flag) = {
    let selected_text = view_of_text(selected);
    let selected_tiles =
      List.map(
        view_of_tile(~font_metrics),
        Measured.shift(- offset_l, selected_tiles),
      );
    let caret = Decoration.Caret.view(~font_metrics, offset_t, []);
    let flag =
      Node.span(
        [
          Attr.classes(["flag"]),
          Attr.create(
            "style",
            Printf.sprintf(
              "left: %fpx; top: %fpx; padding: 0 %fpx;",
              (Float.of_int(offset_t) +. 0.5) *. font_metrics.col_width,
              (-1.3) *. font_metrics.row_height,
              0.5 *. font_metrics.col_width,
            ),
          ),
        ],
        [
          Node.div(
            [
              Attr.classes(["selection-box", "moving"]),
              Attr.create(
                "style",
                Printf.sprintf(
                  "left: 0; top: calc(%fpx + 2px); width: %fpx; height: %fpx;",
                  (-0.15) *. font_metrics.row_height,
                  font_metrics.col_width *. Float.of_int(offset_r - offset_l),
                  font_metrics.row_height *. 1.3,
                ),
              ),
            ],
            [],
          ),
          ...selected_tiles,
        ]
        @ [selected_text],
      );
    (caret, flag);
  };
  let (selected_tiles, target_tiles) =
    TupleUtil.map2(
      List.map(view_of_tile(~font_metrics)),
      (selected_tiles, target_tiles),
    );
  List.concat([
    empty_holes,
    selected_tiles,
    target_tiles,
    [text, placeholder, caret, flag],
  ]);
};

let view = (~font_metrics: FontMetrics.t, edit_state: EditState.t) => {
  let (mode, e) =
    switch (EditState.zip_up(edit_state)) {
    | (_, `Typ(_) | `Pat(_)) => failwith("expected expression at top level")
    | (mode, `Exp(e, _)) => (mode, e)
    };
  let vs =
    switch (mode) {
    | Normal(focus) => view_of_normal(~font_metrics, focus, e)
    | Selecting(selection) => view_of_selecting(~font_metrics, selection, e)
    | Restructuring(selection, target) =>
      view_of_restructuring(~font_metrics, selection, target, e)
    };
  Node.div([Attr.id("code")], vs);
};
