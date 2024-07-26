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

let perform = (s: string, z: Zipper.t) => {
  List.iter(Effects.remove, Zipper.Cursor.flatten(z.cur));
  let (toks, r, ctx) = relabel(s, z.ctx);
  let (cell, ctx) =
    toks
    |> List.fold_left((ctx, tok) => mold(ctx, tok), ctx)
    |> remold(~fill=Cell.point(Focus));
  Zipper.unzip(cell, ~ctx)
  |> Option.map(Move.hstep_n(- r))
  |> Option.value(~default=Zipper.mk_unroll(R, cell, ~ctx));
};
