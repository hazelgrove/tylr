open Virtual_dom.Vdom;
open Util;
open Core;

let is_var = s => Re.Str.(string_match(regexp("^[a-z]$"), s, 0));
let is_num = s => Re.Str.(string_match(regexp("^[0-9]$"), s, 0));

let p = a => Update.PerformAction(a);

let arrow_l_r = (key, evt, zipper: Zipper.t): list(Update.t) => {
  let d: Direction.t = key == "ArrowLeft" ? Left : Right;
  if (JsUtil.held(Shift, evt)) {
    switch (zipper) {
    | (Pointing(_), _) => [p(Mark), p(Move(d))]
    | (Selecting(_), _) => [p(Move(d))]
    | (Restructuring(_), _) => []
    };
  } else {
    switch (zipper) {
    | (Selecting(_), _) => [Update.escape(~d, ())]
    | _ => [p(Move(d))]
    };
  };
};

let handlers = (~inject: Update.t => Event.t, ~zipper: Zipper.t) => [
  Attr.on_keypress(_ => Event.Prevent_default),
  Attr.on_keyup(evt =>
    Event.Many(
      switch (JsUtil.get_key(evt), zipper) {
      | ("Shift", (Selecting([], _), _)) => [
          inject(Update.escape()),
          Event.Prevent_default,
        ]
      | ("Alt", _) => [inject(SetShowNeighborTiles(false))]
      | _ => []
      },
    )
  ),
  Attr.on_keydown(evt => {
    let key = JsUtil.get_key(evt);
    let held = m => JsUtil.held(m, evt);
    let updates: list(Update.t) =
      if (!held(Ctrl) && !held(Alt) && !held(Meta)) {
        switch (key) {
        | "ArrowLeft"
        | "ArrowRight" => arrow_l_r(key, evt, zipper)
        | "ArrowUp" =>
          switch (zipper) {
          | (Selecting(_), _) => [p(Mark)]
          | _ => []
          }
        | "ArrowDown" =>
          switch (zipper) {
          | (Restructuring(_), _) => [p(Mark)]
          | _ => []
          }
        | "Backspace"
        | "Delete" => [p(Delete)]
        | "+" => [p(Construct(Exp(Plus)))]
        | "*" => [p(Construct(Exp(Times)))]
        | "(" =>
          switch (zipper) {
          | (_, Pat(_)) => [p(Construct(Pat(Paren([OpHole]))))]
          | (_, Exp(_)) => [p(Construct(Exp(Paren([OpHole]))))]
          }
        | "\\" => [p(Construct(Exp(Lam([OpHole]))))]
        | "=" => [p(Construct(Exp(Let([OpHole], [OpHole]))))]
        | "," =>
          switch (zipper) {
          | (_, Pat(_)) => [p(Construct(Pat(Prod)))]
          | (_, Exp(_)) => [p(Construct(Exp(Prod)))]
          }
        | " " => [p(Construct(Exp(Ap)))]
        | "Escape" => [Update.escape()]
        | _ when is_num(key) => [
            p(Construct(Exp(Num(int_of_string(key))))),
          ]
        | _ when is_var(key) =>
          switch (zipper) {
          | (_, Pat(_)) => [p(Construct(Pat(Var(key))))]
          | (_, Exp(_)) => [p(Construct(Exp(Var(key))))]
          }
        | _ => []
        };
      } else if (held(Ctrl) && !held(Alt)) {
        switch (key) {
        | "z" => [Undo]
        | "Z" => [Redo]
        | _ => []
        };
      } else if (held(Alt) && !held(Ctrl) && !held(Meta)) {
        switch (key) {
        | "Alt" => [SetShowNeighborTiles(true)]
        | "ArrowLeft"
        | "ArrowRight" => arrow_l_r(key, evt, zipper)
        | _ => []
        };
      } else {
        [];
      };
    switch (updates) {
    | [] => Event.Many([])
    | [_, ..._] =>
      Event.Many([Event.Prevent_default, ...List.map(inject, updates)])
    };
  }),
];
