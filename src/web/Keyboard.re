open Virtual_dom.Vdom;
open Util;
open Core;

let is_var = s => Re.Str.(string_match(regexp("^[a-z]$"), s, 0));
let is_num = s => Re.Str.(string_match(regexp("^[0-9]$"), s, 0));

let is_printable = s => Re.Str.(string_match(regexp("^[ -~]$"), s, 0));

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
      | ("Shift", (Selecting(_, [], _), _)) => [
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
    let frame_sort = Frame.sort(snd(zipper));
    let updates: list(Update.t) =
      if (!held(Ctrl) && !held(Alt) && !held(Meta)) {
        switch (key) {
        | "ArrowLeft"
        | "ArrowRight" => arrow_l_r(key, evt, zipper)
        | "ArrowUp" =>
          switch (zipper) {
          | (Pointing(_) | Selecting(_, [], _), _) => [
              FailedInput(Failure(Cant_move)),
            ]
          | (Selecting(_, [_, ..._], _), _) => [p(Mark)]
          | (Restructuring((_, rframe)), _) =>
            switch (rframe) {
            | ([Selection(selection), ..._], _)
                when Option.is_some(Selection.is_restructurable(selection)) => [
                p(Move(Left)),
              ]
            | (_, [Selection(selection), ..._])
                when Option.is_some(Selection.is_restructurable(selection)) => [
                p(Move(Right)),
              ]
            | _ => [FailedInput(Failure(Cant_move))]
            }
          }
        | "ArrowDown" =>
          switch (zipper) {
          | (Pointing(_) | Selecting(_), _) => [
              FailedInput(Failure(Cant_move)),
            ]
          | (Restructuring(_), _) => [p(Mark)]
          }
        | "Backspace" => [p(Delete(Left))]
        | "Delete" => [p(Delete(Right))]
        | "+" => [p(Construct(Tile(Exp(Plus))))]
        | "-" => [p(Construct(Tile(Exp(Minus))))]
        | "*" => [p(Construct(Tile(Exp(Times))))]
        | "/" => [p(Construct(Tile(Exp(Div))))]
        | "(" =>
          switch (fst(zipper)) {
          | Restructuring((
              (_, [Shard(Pat(Paren_l) | Exp(Paren_l))], _),
              _,
            )) => [
              p(Mark),
            ]
          | _ =>
            switch (frame_sort) {
            | Pat => [p(Construct(Shard(Pat(Paren_l))))]
            | Exp => [p(Construct(Shard(Exp(Paren_l))))]
            }
          }
        | ")" =>
          switch (fst(zipper)) {
          | Restructuring((
              (_, [Shard(Pat(Paren_r) | Exp(Paren_r))], _),
              _,
            )) => [
              p(Mark),
            ]
          | _ =>
            switch (frame_sort) {
            | Pat => [p(Construct(Shard(Pat(Paren_r))))]
            | Exp => [p(Construct(Shard(Exp(Paren_r))))]
            }
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
        | "?" =>
          switch (fst(zipper)) {
          | Restructuring(((_, [Shard(Exp(Cond_que))], _), _)) => [
              p(Mark),
            ]
          | _ => [p(Construct(Shard(Exp(Cond_que))))]
          }
        | ":" =>
          switch (fst(zipper)) {
          | Restructuring(((_, [Shard(Exp(Cond_col))], _), _)) => [
              p(Mark),
            ]
          | _ => [p(Construct(Shard(Exp(Cond_col))))]
          }
        | "!" => [p(Construct(Tile(Exp(Fact))))]
        | _ when is_num(key) => [
            p(Construct(Tile(Exp(Num(int_of_string(key)))))),
          ]
        | _ when is_var(key) =>
          switch (zipper) {
          | (_, Pat(_)) => [p(Construct(Tile(Pat(Var(key)))))]
          | (_, Exp(_)) => [p(Construct(Tile(Exp(Var(key)))))]
          }
        | _ when is_printable(key) => [FailedInput(Unrecognized)]
        | _ => []
        };
      } else if (! Os.is_mac^ && held(Ctrl) && !held(Alt) && !held(Meta)) {
        switch (key) {
        | "z"
        | "Z" => held(Shift) ? [Redo] : [Undo]
        | _ => []
        };
      } else if (Os.is_mac^ && held(Meta) && !held(Alt) && !held(Ctrl)) {
        switch (key) {
        | "z"
        | "Z" => held(Shift) ? [Redo] : [Undo]
        | _ => []
        };
      } else if (held(Alt) && !held(Ctrl) && !held(Meta)) {
        switch (key) {
        | "Alt" => [SetShowNeighborTiles(true)]
        | "ArrowLeft"
        | "ArrowRight" => arrow_l_r(key, evt, zipper)
        | _ => []
        };
      } else if (held(Ctrl) && !held(Alt) && !held(Meta)) {
        switch (key) {
        | "t" => [ToggleShowNeighborTiles]
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
