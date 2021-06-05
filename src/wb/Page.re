open Virtual_dom.Vdom;
open Cor;

let is_var = s => Re.Str.string_match(Re.Str.regexp("[a-z]"), s, 0);
let is_num = s => Re.Str.string_match(Re.Str.regexp("[0-9]"), s, 0);

let key_handlers = (~inject: Update.t => Event.t, ~zipper: Zipper.t) => {
  [
    Attr.on_keypress(_ => Event.Prevent_default),
    Attr.on_keyup(evt =>
      Event.Many(
        switch (JsUtil.get_key(evt), zipper) {
        | ("Shift", (Selecting([], _), _)) => [inject(Update.escape())]
        | _ => []
        },
      )
    ),
    Attr.on_keydown(evt => {
      let key = JsUtil.get_key(evt);
      let held = m => JsUtil.held(m, evt);
      let p = a => Update.PerformAction(a);
      let updates: list(Update.t) =
        if (!held(Ctrl) && !held(Alt) && !held(Meta)) {
          switch (key) {
          | "Shift" =>
            switch (zipper) {
            | (Pointing(_), _) => [p(Mark)]
            | _ => []
            }
          | "ArrowLeft"
          | "ArrowRight" =>
            let d: Direction.t = key == "ArrowLeft" ? Left : Right;
            /*
             switch (zipper) {
             | (Pointing(_), _) when held(Shift) => [p(Mark), p(Move(d))]
             | (Selecting(_), _) when !held(Shift) => [
                 Update.escape(~d, ()),
               ]
             | _ => [p(Move(d))]
             };
             */
            [p(Move(d))];
          | "Backspace"
          | "Delete" => [p(Delete)]
          | "+" => [p(Construct(Exp(Plus)))]
          | "(" => [p(Construct(Exp(Paren([OpHole]))))]
          | "\\" => [p(Construct(Exp(Lam([OpHole]))))]
          | "=" => [p(Construct(Exp(Let([OpHole], [OpHole]))))]
          | "," =>
            switch (zipper) {
            | (_, Pat(_)) => [p(Construct(Pat(Prod)))]
            | (_, Exp(_)) => [p(Construct(Exp(Prod)))]
            }
          | "Escape" => [Update.escape()]
          | "Enter" => [p(Mark)]
          | _ =>
            let is_var = is_var(key);
            let is_num = is_num(key);
            switch (zipper) {
            | (_, Pat(_)) when is_var => [p(Construct(Pat(Var(key))))]
            | (_, Exp(_)) when is_var => [p(Construct(Exp(Var(key))))]
            | (_, Exp(_)) when is_num => [
                p(Construct(Exp(Num(int_of_string(key))))),
              ]
            | _ => []
            };
          };
        } else if (held(Ctrl) && !held(Alt)) {
          switch (key) {
          | "z" => [Undo]
          | "Z" => [Redo]
          | _ => []
          };
        } else {
          [];
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
  let selem = (sort, shape: Layout.selem_shape, s) =>
    Layout.annot(Selem(sort, shape, Logo), Text(s));
  let l =
    Layout.(
      spaces([
        selem(Some(Exp), ((Convex, 0), (Convex, 0)), "t"),
        selem(Some(Pat), ((Concave, 0), (Convex, 0)), "y"),
        selem(Some(Pat), ((Concave, 0), (Concave, 0)), "l"),
        selem(None, ((Convex, 0), (Concave, 1)), "r"),
      ])
    );
  Code.view_of_layout(~id="logo", ~text_id="logo-text", ~font_metrics, l);
};

let filters =
  NodeUtil.svg(
    Attr.[id("filters")],
    Decoration.[
      Selem.raised_shadow_filter(~sort=Exp, ()),
      Selem.shadow_filter(~sort=Exp, ()),
      Selem.raised_shadow_filter(~sort=Pat, ()),
      Selem.shadow_filter(~sort=Pat, ()),
      Selem.raised_shadow_filter(),
      Selem.shadow_filter(),
      EmptyHole.inset_shadow_filter(~sort=None),
      EmptyHole.thin_inset_shadow_filter(~sort=None),
      EmptyHole.inset_shadow_filter(~sort=Some(Exp)),
      EmptyHole.thin_inset_shadow_filter(~sort=Some(Exp)),
      EmptyHole.inset_shadow_filter(~sort=Some(Pat)),
      EmptyHole.thin_inset_shadow_filter(~sort=Some(Pat)),
    ],
  );

let view =
    (
      ~inject,
      {font_metrics, logo_font_metrics, zipper, history_frame: _}: Model.t,
    ) =>
  Node.div(
    [Attr.id("page")],
    [
      logo(~font_metrics=logo_font_metrics),
      FontSpecimen.view("font-specimen"),
      FontSpecimen.view("logo-font-specimen"),
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
          ...key_handlers(~inject, ~zipper),
        ],
        [Code.view_of_layout(~font_metrics, Layout.mk_zipper(zipper))],
      ),
    ],
  );
