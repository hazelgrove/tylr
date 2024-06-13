open Stds;

module Action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Un(Dir.t)
    | All
    | Wald
    | Meld
    | Move(Move.Action.t);
};

let unselect = (~toward=?, ~save_anchor=false, z: Zipper.t) =>
  switch (z.cur) {
  | Point(_) => z
  | Select({focus: d, range: zigg}) =>
    let onto = Dir.toggle(Option.value(toward, ~default=d));
    let fill = save_anchor ? Cell.point(Anchor) : Cell.empty;
    Zipper.mk(Ctx.push_zigg(~onto, zigg, ~fill, z.ctx));
  };

let select = (d: Dir.t, z: Zipper.t): option(Zipper.t) => {
  open Options.Syntax;
  let b = Dir.toggle(d);
  switch (z.cur) {
  | Point(_) =>
    let+ (tok, ctx) = Ctx.pull(~from=d, z.ctx);
    let (tok, ctx) =
      switch (Token.pull(~from=b, tok)) {
      | None => (tok, ctx)
      | Some((l, r)) =>
        let (c, tok) = Dir.order(b, (l, r));
        (c, Ctx.push(~onto=d, tok, ctx));
      };
    Zipper.mk(~cur=Select({focus: d, range: Zigg.of_tok(tok)}), ctx);
  | Select({focus: side, range: zigg}) =>
    if (side == d) {
      let+ (tok, ctx) = Ctx.pull(~from=d, z.ctx);
      let (tok, ctx) =
        switch (Token.pull(~from=b, tok)) {
        | None => (tok, ctx)
        | Some((l, r)) =>
          let (c, tok) = Dir.order(b, (l, r));
          (c, Ctx.push(~onto=d, tok, ctx));
        };
      let zigg = Zigg.grow(~side, tok, zigg);
      Zipper.mk(~cur=Select({focus: d, range: zigg}), ctx);
    } else {
      let (tok, rest) = Zigg.pull(~side=d, zigg);
      let (tok, cur) =
        switch (Token.pull(~from=b, tok), rest) {
        | (None, None) => (tok, Cursor.Point(Caret.focus()))
        | (None, Some(zigg)) => (
            tok,
            Select(Selection.{focus: side, range: zigg}),
          )
        | (Some((l, r)), None) =>
          let (c, tok) = Dir.order(b, (l, r));
          (c, Select({focus: side, range: Zigg.of_tok(tok)}));
        | (Some((l, r)), Some(zigg)) =>
          let (c, tok) = Dir.order(b, (l, r));
          let zigg = Zigg.push_fail(~side, tok, zigg);
          (c, Select({focus: side, range: zigg}));
        };
      Ctx.push(~onto=b, tok, z.ctx)
      |> Zipper.mk(~cur)
      |> Zipper.button
      |> Option.some;
    }
  };
};

let perform = (a: Action.t, z: Zipper.t): option(Zipper.t) =>
  switch (a) {
  | Un(d) => Some(unselect(~toward=d, z))
  | All
  | Wald
  | Meld => failwith("todo Select.perform")
  | Move(a) =>
    switch (a) {
    | Step(H(d)) => select(d, z)
    | Step(V(d)) => Layout.vstep_focus(d, z)
    | Skip(d2) => Layout.skip_focus(d2, z)
    | Jump(pos) => Layout.jump_focus(pos, z)
    | Hole(_) => failwith("unimplemented")
    }
  };
