open Js_of_ocaml;
open Virtual_dom.Vdom;
open Core;

let is_var = s => Re.Str.string_match(Re.Str.regexp("[a-z]"), s, 0);
let is_num = s => Re.Str.string_match(Re.Str.regexp("[0-9]"), s, 0);

let key_handlers =
    (
      ~inject: Update.t => Event.t,
      ~edit_state as (mode, zipper): EditState.t,
    ) => {
  [
    Attr.on_keypress(_ => Event.Prevent_default),
    Attr.on_keydown(evt => {
      let key = Js.to_string(Js.Optdef.get(evt##.key, () => assert(false)));
      let no_ctrl_alt_meta = JsUtil.no_ctrl_alt_meta(evt);
      let held_shift = JsUtil.held_shift(evt);
      let p = a => Update.PerformAction(a);
      let updates: list(Update.t) =
        if (!no_ctrl_alt_meta) {
          [];
        } else {
          switch (key) {
          | "ArrowLeft"
          | "ArrowRight" =>
            let d: Direction.t = key == "ArrowLeft" ? Left : Right;
            switch (mode) {
            | Normal(_) =>
              held_shift ? [p(Mark), p(Move(d))] : [p(Move(d))]
            | Selecting(_) =>
              held_shift ? [p(Move(d))] : [Escape, p(Move(d))]
            | Restructuring(_) => [p(Move(d))]
            };
          | "Backspace" => [p(Delete(Left))]
          | "Delete" => [p(Delete(Right))]
          | "+" => [p(Construct(Plus))]
          | "(" => [p(Construct(Paren))]
          | "\\" => [p(Construct(Lam))]
          | "=" => [p(Construct(Let))]
          | ":" => [p(Construct(Ann))]
          | "Escape" => [Escape]
          | "Enter" =>
            switch (mode) {
            | Normal(_) => []
            | Selecting(_) => [Escape]
            | Restructuring(_) => [p(Mark)]
            }
          | _ =>
            switch (zipper) {
            | `Typ(_) =>
              if (key == "n") {
                [p(Construct(Num))];
              } else if (key == "b") {
                [p(Construct(Bool))];
              } else {
                [];
              }
            | `Pat(_)
            | `Exp(_) =>
              if (is_var(key)) {
                [p(Construct(Var(key)))];
              } else if (is_num(key)) {
                [p(Construct(NumLit(int_of_string(key))))];
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
  let tile = Code.view_of_tile(~font_metrics);
  let style =
    Decoration.Tile.mk_style(
      ~highlighted=true,
      ~raised=true,
      ~stretched=true,
    );
  let profile = Decoration.Tile.mk_profile(~len=1);
  let t_profile =
    profile(~style=style(~sort=Exp, ()), ~shape=Op(false), ());
  let y_profile = profile(~style=style(~sort=Pat, ()), ~shape=Post(), ());
  let l_profile =
    profile(~style=style(~sort=Typ, ()), ~shape=Bin(false), ());
  let r_profile =
    Decoration.Tessera.{
      shape: Pre(true),
      len: 1,
      closed_children: [],
      style: {
        highlighted: true,
        stretched: true,
        raised: true,
      },
    };
  let r_tessera =
    Decoration.container(
      ~font_metrics,
      ~length=r_profile.len,
      ~cls="tessera",
      ~origin=6,
      Decoration.Tessera.view(r_profile),
    );
  // profile(~style=style(), ~shape=Pre(), ());
  Node.div(
    Attr.[id("logo")],
    [
      tile((0, t_profile)),
      tile((2, y_profile)),
      tile((4, l_profile)),
      r_tessera,
      Node.span([Attr.id("logo-text")], [Node.text("t y l r")]),
    ],
  );
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

let view = (~inject, {font_metrics, logo_font_metrics, edit_state}: Model.t) =>
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
        [Code.view(~font_metrics, edit_state)],
      ),
    ],
  );
