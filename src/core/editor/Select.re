open Stds;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Un(Dir.t)
  | All
  | Wald
  | Meld
  | Move(Move.t);

let unselect = (~toward=?, ~save_anchor=false, z: Zipper.t) =>
  switch (z.cur) {
  | Point(_) => z
  | Select({focus: d, range: zigg}) =>
    let onto = Dir.toggle(Option.value(toward, ~default=d));
    let fill = save_anchor ? Cell.point(Anchor) : Cell.empty;
    Zipper.mk(Ctx.push_zigg(~onto, zigg, ~fill, z.ctx));
  };

let hstep_tok = (d: Dir.t, tok: Token.t): Result.t(Token.t, Token.t) => {
  let (m, n) = (Token.length(tok), Utf8.length(tok.text));
  let (l, r) = (1, Token.is_complete(tok) ? m - 1 : n);
  switch (tok.marks) {
  | _ when m <= 1 => Error(Token.clear_marks(tok))
  | None =>
    let car = Caret.focus(Dir.pick(d, (r, l)));
    Ok(Token.put_cursor(Point(car), tok));
  | Some(Point({hand: Anchor, _} as anc)) =>
    let foc = Caret.focus(Dir.pick(d, (r, l)));
    let cur = Step.Cursor.mk(foc, anc);
    Ok(Token.put_cursor(cur, tok));
  | Some(Point({hand: Focus, _} as foc)) =>
    if (Dir.pick(d, (foc.path <= l, foc.path >= r))) {
      Error(Token.clear_marks(tok));
    } else {
      let foc = Step.Caret.shift(Dir.pick(d, ((-1), 1)), foc);
      Ok(Token.put_cursor(Point(foc), tok));
    }
  | Some(Select(sel)) =>
    let (foc, anc) = Dir.order(sel.focus, Step.Selection.carets(sel));
    if (Dir.pick(d, (foc.path <= l, foc.path >= r))) {
      Error(Token.put_cursor(Point(anc), tok));
    } else {
      let foc = Step.Caret.shift(Dir.pick(d, ((-1), 1)), foc);
      let cur = Step.Cursor.mk(foc, anc);
      Ok(Token.put_cursor(cur, tok));
    };
  };
};

let hstep = (d: Dir.t, z: Zipper.t): option(Zipper.t) => {
  open Options.Syntax;
  let b = Dir.toggle(d);
  switch (z.cur) {
  | Point(_) =>
    let (delim, ctx) = Ctx.pull(~from=d, z.ctx);
    let+ tok = Delim.is_tok(delim);
    // let+ (tok, ctx) = Ctx.pull(~from=d, z.ctx);
    let (tok, ctx) =
      switch (hstep_tok(d, tok)) {
      | Error(tok) => (tok, ctx)
      | Ok(tok) => (tok, Ctx.push(~onto=d, tok, z.ctx))
      };
    Zipper.mk(~cur=Select({focus: d, range: Zigg.of_tok(tok)}), ctx);
  | Select({focus: side, range: zigg}) =>
    if (side == d) {
      let (delim, ctx) = Ctx.pull(~from=d, z.ctx);
      let+ tok = Delim.is_tok(delim);
      // let+ (tok, ctx) = Ctx.pull(~from=d, z.ctx);
      let (tok, ctx) =
        switch (hstep_tok(d, tok)) {
        | Error(tok) => (tok, ctx)
        | Ok(tok) => (tok, Ctx.push(~onto=d, tok, z.ctx))
        };
      let zigg = Zigg.grow(~side, tok, zigg);
      Zipper.mk(~cur=Select({focus: d, range: zigg}), ctx);
    } else {
      let (tok, rest) = Zigg.pull(~side, zigg);
      let (tok, cur) =
        switch (hstep_tok(d, tok), rest) {
        | (Error(tok), None) => (tok, Cursor.Point(Caret.focus()))
        | (Error(tok), Some(zigg)) => (
            tok,
            Select(Selection.{focus: side, range: zigg}),
          )
        | (Ok(tok), None) => (
            tok,
            Select({focus: side, range: Zigg.of_tok(tok)}),
          )
        | (Ok(tok), Some(zigg)) => (
            tok,
            Select({focus: side, range: Zigg.grow(~side, tok, zigg)}),
          )
        };
      Ctx.push(~onto=b, tok, z.ctx)
      |> Zipper.mk(~cur)
      |> Zipper.button
      |> Option.some;
    }
  };
};

let perform = (a: t, z: Zipper.t): option(Zipper.t) =>
  switch (a) {
  | Un(d) => Some(unselect(~toward=d, z))
  | All
  | Wald
  | Meld => failwith("todo Select.perform")
  | Move(a) =>
    switch (a) {
    | Step(H(d)) => hstep(d, z)
    | Step(V(d)) => Move.vstep(d, z)
    | Skip(d2) => Move.skip(d2, z)
    | Jump(pos) => Move.jump(pos, z)
    | Hole(_) => failwith("unimplemented")
    }
  };
