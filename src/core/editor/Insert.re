open Stds;

let mold = (ctx: Ctx.t, tok: Token.Unmolded.t): Ctx.t => {
  let ((dn, up), tl) = Ctx.uncons(ctx);
  let (l, r) = Ctx.Tl.bounds(tl);
  switch (Molder.mold(~bound=l, dn, tok)) {
  | None => ctx
  | Some(Neq(dn)) => Ctx.cons((dn, up), tl)
  | Some(Eq(l)) =>
    let (dn, up) = ([l], Slope.cat(up, Bound.to_list(r)));
    Ctx.map_hd(Frame.Open.cat((dn, up)), Ctx.Tl.rest(tl));
  };
};

let rec remold = (~fill=Cell.empty, ctx: Ctx.t): (Cell.t, Ctx.t) => {
  let ((dn, up), tl) = Ctx.uncons(ctx);
  switch (Slope.unlink(up)) {
  | Some((tok, cell, up)) when Token.Grout.is(tok) =>
    Effects.remove(tok);
    let up = Slope.cat(Slope.Up.unroll(cell), up);
    remold(~fill, Ctx.cons((dn, up), tl));
  | _ =>
    let (l, r) = Ctx.Tl.bounds(tl);
    switch (up) {
    | [] =>
      let ctx = Ctx.cons(Frame.Open.empty, tl);
      (Melder.complete_bounded(~bounds=(l, r), ~onto=L, dn, ~fill), ctx);
    | [hd, ...up_tl] =>
      let (molded, rest) = Molder.remold(~bound=l, dn, ~fill, hd);
      let (fill, up) =
        switch (rest) {
        | Ok(cell) => (cell, up_tl)
        | Error(up) =>
          let cell = Option.is_none(molded) ? fill : Cell.empty;
          (cell, Slope.cat(up, up_tl));
        };
      let ctx =
        switch (molded) {
        | None => Ctx.cons((dn, up), tl)
        | Some(Neq(dn)) => Ctx.cons((dn, up), tl)
        | Some(Eq(l)) =>
          let (dn, up) = ([l], Slope.cat(up, Bound.to_list(r)));
          Ctx.map_hd(Frame.Open.cat((dn, up)), Ctx.Tl.rest(tl));
        };
      remold(~fill, ctx);
    };
  };
};

let pull_text = (~from, ctx) =>
  switch (Ctx.pull(~from, ctx)) {
  | Some((tok, ctx)) when tok.text != "" => (
      Token.affix(~side=from, tok),
      ctx,
    )
  | _ => ("", ctx)
  };
let relabel = (s: string, ctx: Ctx.t): (list(Token.Unmolded.t), int, Ctx.t) => {
  let (l, ctx) = pull_text(~from=L, ctx);
  let (r, ctx) = pull_text(~from=R, ctx);
  (Labeler.label(l ++ s ++ r), Stds.Utf8.length(r), ctx);
};

let extend_tok = (~side=Dir.R, s: string, tok: Token.t) =>
  switch (tok.mtrl) {
  | Tile((lbl, _)) =>
    let (l, r) =
      Token.split_text(tok)
      |> Option.map(((l, _, r)) => (l, r))
      |> Option.value(
           ~default=Dir.pick(side, (("", tok.text), (tok.text, ""))),
         );
    let text = l ++ s ++ r;
    Labeler.label(text) |> List.exists(Token.Unmolded.has_lbl(lbl))
      ? Some({
          ...tok,
          text,
          marks: Some(Point(Caret.focus(Utf8.length(l ++ s)))),
        })
      : None;
  | Grout(_)
  | Space () => None
  };

let try_extend_tok = (s: string, ctx: Ctx.t): option(Ctx.t) => {
  open Options.Syntax;
  let l = Ctx.face(~side=L, ctx);
  let r = Ctx.face(~side=R, ctx);
  switch (l, r) {
  | (Some(l), Some(r)) when Option.is_some(Token.merge(l, r)) =>
    let tok = Option.get(Token.merge(~save_cursor=L, l, r));
    let+ tok = extend_tok(s, tok);
    ctx
    |> Ctx.map_face(~side=L, Fun.const(tok))
    |> Ctx.map_face(~side=R, Fun.const(tok));
  | _ =>
    let extend = (side: Dir.t, face) => {
      let* face = face;
      let+ tok = extend_tok(~side=Dir.toggle(side), s, face);
      ctx |> Ctx.map_face(~side, Fun.const(tok));
    };
    let/ () = extend(L, l);
    extend(R, r);
  };
};

let perform = (s: string, z: Zipper.t) => {
  switch (z.cur, try_extend_tok(s, z.ctx)) {
  | (Point(_), Some(ctx)) => Zipper.mk(ctx)
  | _ =>
    List.iter(Effects.remove, Zipper.Cursor.flatten(z.cur));
    let (toks, r, ctx) = relabel(s, z.ctx);
    let (cell, ctx) =
      toks
      |> List.fold_left(
           (ctx, tok) => {
             P.log("-- molding --");
             P.show("ctx", Ctx.show(ctx));
             P.show("tok", Token.Unmolded.show(tok));
             mold(ctx, tok);
           },
           ctx,
         )
      |> remold(~fill=Cell.point(Focus));
    Zipper.unzip(cell, ~ctx)
    |> Option.map(Move.hstep_n(- r))
    |> Option.value(~default=Zipper.mk_unroll(R, cell, ~ctx));
  };
};
