open Virtual_dom.Vdom;
open Util;
open Cor;

let is_var = s => Re.Str.string_match(Re.Str.regexp("[a-z]"), s, 0);
let is_num = s => Re.Str.string_match(Re.Str.regexp("[0-9]"), s, 0);

let handlers = (~inject: Update.t => Event.t, ~zipper: Zipper.t) => [
  Attr.on_keypress(_ => Event.Prevent_default),
  Attr.on_keyup(evt =>
    Event.Many(
      switch (JsUtil.get_key(evt), zipper) {
      | ("Alt", (Selecting(_, [], _), _)) => [
          inject(Update.escape()),
          Event.Prevent_default,
        ]
      | _ => []
      },
    )
  ),
  Attr.on_keydown(evt => {
    let key = JsUtil.get_key(evt);
    let held = m => JsUtil.held(m, evt);
    let p = a => Update.PerformAction(a);
    let frame_sort = Frame.sort(snd(zipper));
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
        | "Backspace" => [p(Delete(Left))]
        | "Delete" => [p(Delete(Right))]
        | "+" => [p(Construct(Tile(Exp(Plus))))]
        | "*" => [p(Construct(Tile(Exp(Times))))]
        | "(" =>
          switch (frame_sort) {
          | Pat => [p(Construct(Shard(Pat(Paren_l))))]
          | Exp => [p(Construct(Shard(Exp(Paren_l))))]
          }
        | ")" =>
          switch (frame_sort) {
          | Pat => [p(Construct(Shard(Pat(Paren_r))))]
          | Exp => [p(Construct(Shard(Exp(Paren_r))))]
          }
        | "\\" => [p(Construct(Tile(Exp(Lam([OpHole])))))]
        | "=" => [p(Construct(Tile(Exp(Let([OpHole], [OpHole])))))]
        | "," =>
          switch (zipper) {
          | (_, Pat(_)) => [p(Construct(Tile(Pat(Prod))))]
          | (_, Exp(_)) => [p(Construct(Tile(Exp(Prod))))]
          }
        | " " => [p(Construct(Tile(Exp(Ap))))]
        | "Escape" => [Update.escape()]
        | "Enter" =>
          switch (zipper) {
          | (Selecting(_) | Restructuring(_), _) => [p(Mark)]
          | _ => []
          }
        | _ when is_num(key) => [
            p(Construct(Tile(Exp(Num(int_of_string(key)))))),
          ]
        | _ when is_var(key) =>
          switch (zipper) {
          | (_, Pat(_)) => [p(Construct(Tile(Pat(Var(key)))))]
          | (_, Exp(_)) => [p(Construct(Tile(Exp(Var(key)))))]
          }
        | _ => []
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
