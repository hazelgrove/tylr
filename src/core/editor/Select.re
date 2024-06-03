open Util;

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
    let fill = save_anchor ? Fill.unit(Cell.point(Anchor)) : Fill.empty;
    Zipper.mk(Melder.Ctx.push_zigg(~onto, zigg, ~fill, z.ctx));
  };

let select = (d: Dir.t, z: Zipper.t): option(Zipper.t) => {
  open Options.Syntax;
  let b = Dir.toggle(d);
  switch (z.cur) {
  | Point(_) =>
    let+ (tok, ctx) = Melder.Ctx.pull(~from=d, z.ctx);
    let (tok, ctx) =
      switch (Token.pull(~from=b, tok)) {
      | None => (tok, ctx)
      | Some((l, r)) =>
        let (c, tok) = Dir.order(b, (l, r));
        (c, Melder.Ctx.push_fail(~onto=d, tok, ctx));
      };
    Zipper.mk(~cur=Select({focus: d, range: Zigg.of_tok(tok)}), ctx);
  | Select({focus: side, range: zigg}) =>
    if (side == d) {
      let+ (tok, ctx) = Melder.Ctx.pull(~from=d, z.ctx);
      let (tok, ctx) =
        switch (Token.pull(~from=b, tok)) {
        | None => (tok, ctx)
        | Some((l, r)) =>
          let (c, tok) = Dir.order(b, (l, r));
          (c, Melder.Ctx.push_fail(~onto=d, tok, ctx));
        };
      let zigg = Melder.Zigg.grow(~side, tok, zigg);
      Zipper.mk(~cur=Select({focus: d, range: zigg}), ctx);
    } else {
      let (tok, rest) = Melder.Zigg.pull(~side=d, zigg);
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
          let zigg = Melder.Zigg.push_fail(~side, tok, zigg);
          (c, Select({focus: side, range: zigg}));
        };
      let ctx = Melder.Ctx.(close(push_fail(~onto=b, tok, z.ctx)));
      Some(Zipper.mk(~cur, ctx));
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
