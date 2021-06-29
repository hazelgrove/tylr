open Virtual_dom.Vdom;
open Util;
open Cor;

let is_var = s => Re.Str.string_match(Re.Str.regexp("[a-z]"), s, 0);
let is_num = s => Re.Str.string_match(Re.Str.regexp("[0-9]"), s, 0);

let key_handlers = (~inject: Update.t => Event.t, ~zipper: Zipper.t) => {
  [
    Attr.on_keypress(_ => Event.Prevent_default),
    Attr.on_keyup(evt =>
      Event.Many(
        switch (JsUtil.get_key(evt), zipper) {
        | ("Alt", (Selecting([], _), _)) => [inject(Update.escape())]
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
          | "ArrowLeft"
          | "ArrowRight" =>
            let d: Direction.t = key == "ArrowLeft" ? Left : Right;
            switch (zipper) {
            | (Selecting(_), _) => [Update.escape(~d, ())]
            | _ => [p(Move(d))]
            };
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
          | "Enter" =>
            switch (zipper) {
            | (Selecting(_) | Restructuring(_), _) => [p(Mark)]
            | _ => []
            }
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
        } else if (!held(Ctrl) && held(Alt) && !held(Meta)) {
          switch (key) {
          | "Alt" => [p(Mark)]
          | "ArrowLeft"
          | "ArrowRight" =>
            let d: Direction.t = key == "ArrowLeft" ? Left : Right;
            switch (zipper) {
            | (Pointing(_), _) => [p(Mark), p(Move(d))]
            | (Selecting(_), _) => [p(Move(d))]
            | (Restructuring(_), _) => []
            };
          | _ => []
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
  let selem = (step, color: Color.t, shape: Layout.selem_shape, s): Layout.t =>
    Layout.annot(Selem({color, shape, step}), Text(s));
  let l =
    Layout.(
      spaces(
        // HACK
        Selected,
        [
          selem(0, Exp, ((Convex, 0), (Convex, 0)), "t"),
          selem(1, Pat, ((Concave, 0), (Convex, 0)), "y"),
          selem(2, Typ, ((Concave, 0), (Concave, 0)), "l"),
          selem(3, Selected, ((Convex, 0), (Concave, 1)), "r"),
        ],
      )
    );
  Code.view_of_layout(
    ~id="logo",
    ~text_id="logo-text",
    ~font_metrics,
    DecPaths.mk(~logo_selems=[0, 1, 2, 3], ()),
    l,
  );
};

let filters =
  NodeUtil.svg(
    Attr.[id("filters")],
    [
      SelemDec.raised_shadow_filter(~color=Exp),
      SelemDec.shadow_filter(~color=Exp),
      SelemDec.raised_shadow_filter(~color=Pat),
      SelemDec.shadow_filter(~color=Pat),
      SelemDec.raised_shadow_filter(~color=Typ),
      SelemDec.shadow_filter(~color=Typ),
      SelemDec.raised_shadow_filter(~color=Selected),
      SelemDec.shadow_filter(~color=Selected),
      EmptyHoleDec.inset_shadow_filter(~color=Selected),
      EmptyHoleDec.thin_inset_shadow_filter(~color=Selected),
      EmptyHoleDec.inset_shadow_filter(~color=Exp),
      EmptyHoleDec.thin_inset_shadow_filter(~color=Exp),
      EmptyHoleDec.inset_shadow_filter(~color=Pat),
      EmptyHoleDec.thin_inset_shadow_filter(~color=Pat),
      CaretPosDec.blur_filter,
    ],
  );

let view =
    (
      ~inject,
      {font_metrics, logo_font_metrics, zipper, history_frame: _}: Model.t,
    ) => {
  let (subject, _) = zipper;
  let dpaths = DecPaths.of_zipper(zipper);
  let l = Layout.mk_zipper(zipper);
  // let rail_color = {
  //   let frame_color = Color.of_sort(Frame.sort(frame));
  //   switch (subject) {
  //   | Pointing(_) => frame_color
  //   | Selecting(_)
  //   | Restructuring(_) => Selected
  //   // | Restructuring(selection, _) =>
  //   //   Selection.is_whole_any(selection) ? frame_color : Selected
  //   };
  // };
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
        [
          BarDec.view(~font_metrics),
          Code.view_of_layout(
            ~id="under-the-rail",
            ~text_id="under-the-rail-text",
            ~font_metrics,
            ~subject,
            dpaths,
            l,
          ),
        ],
      ),
    ],
  );
};
