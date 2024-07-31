open Stds;

exception Bug__lost_cursor;

let mold = (ctx: Ctx.t, tok: Token.Unmolded.t): Ctx.t => {
  let ((dn, up), tl) = Ctx.uncons(ctx);
  let (l, r) = Ctx.Tl.bounds(tl);
  switch (Molder.mold(~bound=l, dn, tok)) {
  | Removed => ctx
  | Molded(Neq(dn))
  | Deferred(Neq(dn)) => Ctx.cons((dn, up), tl)
  | Molded(Eq(l))
  | Deferred(Eq(l)) =>
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
          let cell = molded == Removed ? fill : Cell.empty;
          (cell, Slope.cat(up, up_tl));
        };
      let ctx =
        switch (molded) {
        | Removed => Ctx.cons((dn, up), tl)
        | Molded(Neq(dn))
        | Deferred(Neq(dn)) => Ctx.cons((dn, up), tl)
        | Molded(Eq(l))
        | Deferred(Eq(l)) =>
          let (dn, up) = ([l], Slope.cat(up, Bound.to_list(r)));
          Ctx.map_hd(Frame.Open.cat((dn, up)), Ctx.Tl.rest(tl));
        };
      remold(~fill, ctx);
    };
  };
};

let pull_text = (~from, ctx) =>
  switch (Ctx.pull(~from, ctx)) {
  | (Node(tok), ctx) when tok.text != "" => (
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

let extend = (~side=Dir.R, s: string, tok: Token.t) =>
  switch (tok.mtrl) {
  | Space(_)
  | Grout(_) => None
  | Tile((lbl, _)) =>
    let (l, r) =
      Token.split_text(tok)
      |> Option.map(((l, _, r)) => (l, r))
      |> Option.value(
           ~default=Dir.pick(side, (("", tok.text), (tok.text, ""))),
         );
    let text = l ++ s ++ r;
    switch (Labeler.label(text)) {
    | [t] when Token.Unmolded.has_lbl(lbl, t) =>
      // let _ = failwith("todo: only add cursor if within token");
      let n = Utf8.length(l ++ s);
      let extended = {...tok, text};
      extended
      |> (
        n >= Token.length(extended)
          ? Token.clear_marks : Token.put_cursor(Point(Caret.focus(n)))
      )
      |> Option.some;
    | _ => None
    };
  };
let try_extend = (~side: Dir.t, s: string, z: Zipper.t): option(Zipper.t) => {
  open Options.Syntax;
  let* _ = Cursor.get_point(z.cur);
  let* () = Options.of_bool(!Strings.is_empty(s));
  let (face, ctx) = Ctx.pull_face(~from=side, z.ctx);
  let* tok = Delim.is_tok(face);
  let+ tok = extend(~side=Dir.toggle(side), s, tok);
  // let (l, _, r) = Token.unzip(tok);
  // // expecting extend to leave nonempty prefix before caret
  // let tok = Option.get(l);
  // let ctx =
  //   switch (r) {
  //   | None => Ctx.push(~onto=L, tok, ctx)
  //   | Some(_) => ctx |> Ctx.push(~onto=L, tok) |> Ctx.push(~onto=R, tok)
  //   };
  // Zipper.mk(ctx);
  ctx |> Ctx.push(~onto=L, tok) |> Zipper.mk;
};

// maybe rename expandable
let expand = (tok: Token.t) =>
  switch (tok.mtrl) {
  | Space(White)
  | Grout(_)
  | Tile((Const(_), _)) => None
  | Space(Unmolded)
  | Tile(_) =>
    open Options.Syntax;
    let* labeled = Labeler.single(tok.text);
    Token.Unmolded.expands(labeled);
  };
let try_expand = (s: string, z: Zipper.t): option(Zipper.t) => {
  open Options.Syntax;
  let* () = Options.of_bool(String.starts_with(~prefix=" ", s));
  let (face, ctx) = Ctx.pull_face(~from=L, z.ctx);
  let* tok = Delim.is_tok(face);
  let* tok = expand(tok);
  // todo: unify this better with existing molding pathways
  let ((dn, up), tl) = Ctx.uncons(ctx);
  let (l, r) = Ctx.Tl.bounds(tl);
  let+ ctx =
    switch (Molder.mold(~bound=l, dn, tok)) {
    | Removed
    | Deferred(_) => None
    | Molded(Neq(dn)) => Some(Ctx.cons((dn, up), tl))
    | Molded(Eq(l)) =>
      let (dn, up) = ([l], Slope.cat(up, Bound.to_list(r)));
      Ctx.Tl.rest(tl) |> Ctx.map_hd(Frame.Open.cat((dn, up))) |> Option.some;
    };
  let ctx = Ctx.push(~onto=L, Token.space(), ctx);
  let (cell, ctx) = remold(~fill=Cell.point(Focus), ctx);
  Zipper.unzip(cell, ~ctx) |> Options.get_exn(Bug__lost_cursor);
};

let perform = (s: string, z: Zipper.t) => {
  List.iter(Effects.remove, Zipper.Cursor.flatten(z.cur));
  open Options.Syntax;
  let- () = try_extend(~side=L, s, z);
  let- () = try_extend(~side=R, s, z);
  let- () = try_expand(s, z);

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
  |> Options.get_exn(Bug__lost_cursor);
  // |> Option.value(~default=Zipper.mk_unroll(R, cell, ~ctx));
};
