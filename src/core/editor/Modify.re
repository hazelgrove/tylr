open Stds;

// common utilities for modification edits eg insert and delete

let meld_or_bust = (ctx: Ctx.t, tok: Token.t): Ctx.t => {
  assert(tok.text == "");
  switch (Ctx.push_opt(~onto=L, tok, ctx)) {
  | Some(ctx) => ctx
  | None => ctx
  };
};

let relabel =
    (s: string, ctx: Ctx.t): (Chain.t(Cell.t, Token.Unmolded.t), Ctx.t) => {
  let (l, rest) = Ctx.pull(~from=L, ctx);
  let (r, rest) = Ctx.pull(~from=R, rest);
  let merges = Delim.merges(l, r);
  let s_l =
    Delim.is_tok(l)
    |> Option.map(Token.affix(~side=L))
    |> Option.value(~default="");
  let s_r =
    Delim.is_tok(r)
    |> Option.map(Token.affix(~side=R))
    |> Option.value(~default="");
  let labeled = Labeler.label(s_l ++ s ++ s_r);
  // push left face back if its labeling remains unchanged
  let (labeled, rest) =
    switch (labeled) {
    | [hd, ...tl] when hd.text == s_l && s_l != "" =>
      let ctx =
        Delim.is_tok(l)
        |> Option.map(t => Ctx.push(~onto=L, t, rest))
        |> Option.value(~default=rest);
      (tl, ctx);
    | labeled => (labeled, rest)
    };
  // push right face back if its labeling remains unchanged
  let (labeled, rest) =
    switch (Lists.Framed.ft(labeled)) {
    | Some((pre, ft)) when ft.text == s_r && s_r != "" && !merges =>
      let ctx =
        Delim.is_tok(r)
        |> Option.map(t => Ctx.push(~onto=R, t, rest))
        |> Option.value(~default=rest);
      // (List.rev(pre), 0, ctx);
      (List.rev(pre), ctx);
    | _ =>
      // (labeled, Utf8.length(s_r), rest)
      (labeled, rest)
    };

  // restore caret position
  let n = Utf8.length(s_l ++ s);
  let toks =
    labeled
    |> Lists.fold_map(
         ~init=0,
         ~f=(num_chars, tok: Token.Unmolded.t) => {
           let m = num_chars + Utf8.length(tok.text);
           let tok =
             m >= n
               ? Token.put_cursor(Point(Caret.focus(n - num_chars)), tok)
               : tok;
           (m, tok);
         },
       )
    |> snd
    // normalize the cursors by popping off any carets at the token edges
    // and storing them instead in neighboring cells, the final result being a
    // chain of cell-loops (either empty or with a caret) and token-links
    |> Lists.fold_right(
         ~init=Chain.unit(Cell.empty), ~f=(tok: Token.Unmolded.t, c) =>
         switch (tok.marks) {
         | Some(Point({path: 0, _})) =>
           Chain.link(Cell.point(Focus), Token.clear_marks(tok), c)
         | Some(Point({path: n, _})) when n == Utf8.length(tok.text) =>
           c
           |> Chain.map_hd(Fun.const(Cell.point(Focus)))
           |> Chain.link(Cell.empty, Token.clear_marks(tok))
         | _ => Chain.link(Cell.empty, tok, c)
         }
       );
  (toks, rest);
};

let mold = (ctx: Ctx.t, ~fill=Cell.empty, tok: Token.Unmolded.t): Ctx.t => {
  let ((dn, up), tl) = Ctx.uncons(ctx);
  let (l, r) = Ctx.Tl.bounds(tl);
  switch (Molder.mold(~bound=l, dn, ~fill, tok)) {
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
      let n = Utf8.length(l ++ s);
      let extended = {...tok, text};
      extended
      |> (
        // extending doesn't require zipping (which takes place in other
        // insertion paths while remolding), which handles cursor normalization,
        // so need to do some manual cursor normalization here
        n >= Token.length(extended)
          ? Token.clear_marks : Token.put_cursor(Point(Caret.focus(n)))
      )
      |> Option.some;
    | _ => None
    };
  };
let try_extend = (s: string, z: Zipper.t): option(Zipper.t) => {
  open Options.Syntax;
  let* () = Options.of_bool(!Strings.is_empty(s));
  let (sites, ctx) = Zipper.cursor_site(z);
  let* site = Cursor.get_point(sites);
  let+ (extended, ctx) =
    switch (site) {
    | Within(tok) =>
      let+ extended = extend(~side=R, s, tok);
      (extended, ctx);
    | Between =>
      let/ () = {
        let (face, ctx) = Ctx.pull(~from=L, ctx);
        let* tok = Delim.is_tok(face);
        let+ extended = extend(~side=R, s, tok);
        (extended, ctx);
      };
      let (face, ctx) = Ctx.pull(~from=R, ctx);
      let* tok = Delim.is_tok(face);
      let+ extended = extend(~side=L, s, tok);
      (extended, ctx);
    };
  ctx
  |> Ctx.push(~onto=L, extended)
  |> (Option.is_some(extended.marks) ? Ctx.push(~onto=R, extended) : Fun.id)
  |> Zipper.mk;
};

// maybe rename expandable
let expand = (tok: Token.t) =>
  switch (tok.mtrl) {
  | Space(White)
  | Grout(_) => None
  // | Tile((Const(_), _)) => None
  | Space(Unmolded)
  | Tile(_) =>
    open Options.Syntax;
    let* labeled = Labeler.single(tok.text);
    Token.Unmolded.expands(labeled);
  };
let try_expand = (s: string, z: Zipper.t): option(Zipper.t) => {
  open Options.Syntax;
  let* () = Options.of_bool(String.starts_with(~prefix=" ", s));
  let (face, rest) = Ctx.pull(~from=L, z.ctx);
  let* tok = Delim.is_tok(face);
  let* expanded = expand(tok);
  let ((dn, up), tl) = Ctx.uncons(rest);
  let (l, r) = Ctx.Tl.bounds(tl);
  switch (Molder.mold(~bound=l, dn, expanded)) {
  | Removed
  | Deferred(_) => None
  | Molded(m) when Melder.Melded.face(m).mtrl == tok.mtrl =>
    let ctx = z.ctx |> Ctx.push(~onto=L, Token.space());
    return(Zipper.mk(ctx));
  | Molded(Neq(dn)) =>
    let (cell, ctx) =
      Ctx.cons((dn, up), tl)
      |> Ctx.push(~onto=L, Token.space())
      |> remold(~fill=Cell.point(Focus));
    return(Zipper.unzip_exn(cell, ~ctx));
  | Molded(Eq(l)) =>
    let (dn, up) = ([l], Slope.cat(up, Bound.to_list(r)));
    let ctx = Ctx.Tl.rest(tl) |> Ctx.map_hd(Frame.Open.cat((dn, up)));
    return(Zipper.mk(ctx));
  };
};

let put_edge = (side: Dir.t, tok: Token.t) =>
  switch (side) {
  | L => Token.put_cursor(Point(Caret.focus(0)), tok)
  | R => Token.put_cursor(Point(Caret.focus(Token.length(tok))), tok)
  };

let delete_toks =
    (d: Dir.t, toks: list(Token.t)): Chain.t(Cell.t, Token.Unmolded.t) => {
  let n = List.length(toks);
  toks
  // first, clear text of selected tokens within selection bounds and mark
  // either the first or last token with the final cursor position
  |> List.mapi((i, tok) =>
       if (i == 0 && i == n - 1) {
         // single-token selection
         // note: affixes empty if token completely selected
         let (l, r) = Token.(affix(~side=L, tok), affix(~side=R, tok));
         {...tok, text: l ++ r}
         |> Token.put_cursor(Point(Step.Caret.focus(Utf8.length(l))));
       } else if (i == 0) {
         let l = Token.affix(~side=L, tok);
         let car = Step.Caret.focus(Utf8.length(l));
         {...tok, text: l}
         |> Token.(d == L ? put_cursor(Point(car)) : clear_marks);
       } else if (i == n - 1) {
         let r = Token.affix(~side=R, tok);
         {...tok, text: r}
         |> (d == R ? put_edge(r == "" ? R : L) : Token.clear_marks);
       } else {
         Token.clear_marks({...tok, text: ""});
       }
     )
  // next, normalize the cursors by popping off any carets at the token edges
  // and storing them instead in neighboring cells, the final result being a
  // chain of cell-loops (either empty or with a caret) and token-links
  |> Lists.fold_right(
       ~init=Chain.unit(Cell.empty),
       ~f=(tok, c) => {
         let (l, tok, r) = Token.pop_end_carets(tok);
         c
         |> Chain.map_hd(r == None ? Fun.id : Fun.const(Cell.point(Focus)))
         |> Chain.link(l == None ? Cell.empty : Cell.point(Focus), tok);
       },
     )
  // finally, unmold the tokens
  |> Chain.map_link(Token.unmold);
};

// delete_sel clears the textual content of the current selection (doing nothing if
// the selection is empty). this entails dropping all of the zigg's cells and
// remelding the zigg's tokens as empty ghosts onto (the left side of) the ctx. in
// the case of tokens at the ends of the selection that are split by the selection
// boundaries, the selection-external affixes of those tokens are preserved.
let delete_sel = (d: Dir.t, z: Zipper.t): Zipper.t => {
  switch (z.cur) {
  | Point(_) => z
  | Select(sel) =>
    // prune ctx of any duplicated tokens
    let (_, ctx) = Zipper.cursor_site(z);
    let (molded, fill) =
      Zigg.tokens(sel.range)
      |> delete_toks(d)
      // remold each token against the ctx, using each preceding cell as its fill,
      // and return the total ctx and the final remaining fill to be used when
      // subsequently remolding
      |> Chain.fold_left(
           fill => (ctx, fill),
           ((ctx, fill), tok, next_fill) =>
             (mold(ctx, ~fill, tok), next_fill),
         );
    let (remolded, ctx) = remold(molded, ~fill);
    Zipper.unzip_exn(remolded, ~ctx);
  };
};

let delete = (d: Dir.t, z: Zipper.t) => {
  open Options.Syntax;
  let+ z = Cursor.is_point(z.cur) ? Select.hstep(d, z) : return(z);
  delete_sel(d, z);
};

let insert = (s: string, z: Zipper.t) => {
  open Options.Syntax;
  let z = delete_sel(L, z);

  let- () = try_extend(s, z);
  let- () = try_expand(s, z);

  let (toks, ctx) = relabel(s, z.ctx);
  let (molded, fill) =
    toks
    |> Chain.fold_left(
         fill => (ctx, fill),
         ((ctx, fill), tok, next_fill) =>
           (mold(ctx, ~fill, tok), next_fill),
       );
  let (remolded, ctx) = remold(~fill, molded);
  Zipper.unzip_exn(remolded, ~ctx);
};
