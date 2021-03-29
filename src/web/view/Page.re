open Js_of_ocaml;
open Virtual_dom.Vdom;
open Core;

let is_var = s => Re.Str.string_match(Re.Str.regexp("[a-z]"), s, 0);
let is_num = s => Re.Str.string_match(Re.Str.regexp("[0-9]"), s, 0);

let key_handlers = (~inject: Update.t => Event.t, ~edit_state: EditState.t) => {
  [
    Attr.on_keypress(_ => Event.Prevent_default),
    Attr.on_keyup(evt =>
      Event.Many(
        switch (JsUtil.get_key(evt)) {
        | "Shift"
            when
              EditState.is_selecting(edit_state)
              && EditState.has_no_selection(edit_state) => [
            inject(Escape),
          ]
        | _ => []
        },
      )
    ),
    Attr.on_keydown(evt => {
      let key = JsUtil.get_key(evt);
      let no_ctrl_alt_meta = JsUtil.no_ctrl_alt_meta(evt);
      let held_shift = JsUtil.held_shift(evt);
      let p = a => Update.PerformAction(a);
      let updates: list(Update.t) =
        if (!no_ctrl_alt_meta) {
          [];
        } else {
          switch (key) {
          | "Shift" => EditState.is_pointing(edit_state) ? [p(Mark)] : []
          | "ArrowLeft"
          | "ArrowRight" =>
            let d: Direction.t = key == "ArrowLeft" ? Left : Right;
            if (EditState.is_pointing(edit_state)) {
              held_shift ? [p(Mark), p(Move(d))] : [p(Move(d))];
            } else if (EditState.is_selecting(edit_state)) {
              held_shift ? [p(Move(d))] : [Escape, p(Move(d))];
            } else {
              [p(Move(d))];
            };
          | "Backspace" => [p(Delete(Left))]
          | "Delete" => [p(Delete(Right))]
          | "+" => [p(Construct(Plus))]
          | "(" => [p(Construct(Paren_l))]
          | "\\" => [p(Construct(Lam))]
          | "=" => [p(Construct(Let_eq))]
          | ":" => [p(Construct(Ann))]
          | "Escape" => [Escape]
          | "Enter" =>
            if (EditState.is_pointing(edit_state)) {
              [];
            } else if (EditState.is_selecting(edit_state)) {
              [Escape];
            } else {
              [p(Mark)];
            }
          | _ =>
            switch (edit_state) {
            | Typ(_) =>
              if (key == "n") {
                [p(Construct(Text("num")))];
              } else if (key == "b") {
                [p(Construct(Text("bool")))];
              } else {
                [];
              }
            | Pat(_)
            | Exp(_) =>
              if (is_var(key) || is_num(key)) {
                [p(Construct(Text(key)))];
              } else {
                [];
              }
            }
          };
        };
      switch (updates) {
      | [] => Event.Many([])
      | [_, ..._] =>
        Event.(
          Many([
            Prevent_default,
            Stop_propagation,
            ...List.map(inject, updates),
          ])
        )
      };
    }),
  ];
};

let focus_code = () => {
  JsUtil.get_elem_by_id("code-container")##focus;
};

let logo = (~font_metrics) => {
  let tile = (shape: Layout.tile_shape, style: Layout.tile_style, s) =>
    Layout.annot(Tile(shape, style), Text(s));
  let style =
    Layout.mk_tile_style(~highlighted=true, ~raised=true, ~stretched=true);
  let l =
    Layout.(
      grouts([
        tile(Op(false), style(~sort=Exp, ()), "t"),
        tile(Post(), style(~sort=Pat, ()), "y"),
        tile(Bin(false), style(~sort=Typ, ()), "l"),
        Annot(
          Tessera(
            Pre(true),
            {highlighted: true, stretched: true, raised: true},
          ),
          Text("r"),
        ),
      ])
    );
  Code.view_of_layout(~id="logo", ~text_id="logo-text", ~font_metrics, l);
};

let filters =
  NodeUtil.svg(
    Attr.[id("filters")],
    Decoration.[
      Tile.raised_shadow_filter(~sort=Exp, ()),
      Tile.shadow_filter(~sort=Exp, ()),
      Tile.raised_shadow_filter(~sort=Pat, ()),
      Tile.shadow_filter(~sort=Pat, ()),
      Tile.raised_shadow_filter(~sort=Typ, ()),
      Tile.shadow_filter(~sort=Typ, ()),
      Tile.raised_shadow_filter(),
      Tile.shadow_filter(),
      Tessera.raised_shadow_filter,
      Tessera.shadow_filter,
      EmptyHole.inset_shadow_filter,
      EmptyHole.thin_inset_shadow_filter,
    ],
  );

let view =
    (
      ~inject,
      {font_metrics, logo_font_metrics, edit_state, history_frame: _}: Model.t,
    ) =>
  Node.div(
    [Attr.id("page")],
    [
      logo(~font_metrics=logo_font_metrics),
      FontSpecimen.view(),
      LogoFontSpecimen.view(),
      filters,
      Node.div(
        [
          Attr.id("code-container"),
          // necessary to make cell focusable
          Attr.create("tabindex", "0"),
          Attr.on_blur(_ => {
            focus_code();
            Event.Prevent_default;
          }),
          ...key_handlers(~inject, ~edit_state),
        ],
        [
          Code.view_of_layout(
            ~font_metrics,
            Layout_edit_state.mk(edit_state),
          ),
        ],
      ),
    ],
  );
