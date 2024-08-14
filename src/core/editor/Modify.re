open Stds;

// common utilities for modification edits eg insert and delete

let meld_or_bust = (ctx: Ctx.t, tok: Token.t): Ctx.t => {
  assert(tok.text == "");
  switch (Ctx.push_opt(~onto=L, tok, ctx)) {
  | Some(ctx) => ctx
  | None => ctx
  };
};

let relabel = (s: string, ctx: Ctx.t): (list(Token.Unmolded.t), int, Ctx.t) => {
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
  // push left face back if its labeling remains unchanged
  let (labeled, rest) =
    switch (Labeler.label(s_l ++ s ++ s_r)) {
    | [hd, ...tl] when hd.text == s_l && s_l != "" =>
      let ctx =
        Delim.is_tok(l)
        |> Option.map(t => Ctx.push(~onto=L, t, rest))
        |> Option.value(~default=rest);
      (tl, ctx);
    | labeled => (labeled, rest)
    };
  // push right face back if its labeling remains unchanged
  switch (Lists.Framed.ft(labeled)) {
  | Some((pre, ft)) when ft.text == s_r && s_r != "" && !merges =>
    let ctx =
      Delim.is_tok(r)
      |> Option.map(t => Ctx.push(~onto=R, t, rest))
      |> Option.value(~default=rest);
    (List.rev(pre), 0, ctx);
  | _ => (labeled, Utf8.length(s_r), rest)
  };
};

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

let finalize = (~adjust=0, ~fill=Cell.point(Focus), ctx: Ctx.t) => {
  let (cell, ctx) = remold(~fill, ctx);
  Zipper.unzip_exn(cell, ~ctx) |> Move.hstep_n(adjust);
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

// delete_sel clears the textual content of the current selection (doing nothing if
// the selection is empty). specifically, this entails dropping all of the zigg's
// cells and remelding the zigg's tokens as empty ghosts onto (the left side of) the
// ctx. in the case of tokens at the ends of the selection that are split by the
// selection boundaries, the selection-external affixes of those tokens are
// preserved. more specifically, a boundary-split token on the left will retain its
// mold and its left textual affix, while the right affix of a boundary-split token
// on the right end of the selection is returned to the caller for subsequent
// processing.
let delete_sel = (z: Zipper.t): (Ctx.t, string) => {
  // List.iter(Effects.remove, Zipper.Cursor.flatten(z.cur));
  switch (z.cur) {
  | Point(_) => (z.ctx, "")
  | Select(sel) =>
    switch (Zigg.tokens(sel.range)) {
    | [] => assert(false)
    | [tok] =>
      switch (tok.marks) {
      // tok completely selected
      | None => (Ctx.push(~onto=L, {...tok, text: ""}, z.ctx), "")
      // tok partially selected
      | Some(_cur) =>
        let (l, r) =
          switch (Ctx.(pull(~from=L, z.ctx), pull(~from=R, z.ctx))) {
          | ((Node(l), _), (Node(r), _))
              when Token.(merges(tok, l) && merges(tok, r)) =>
            Token.(affix(~side=L, tok), affix(tok, ~side=R))
          | ((Node(l), _), _) when Token.merges(l, tok) =>
            Token.(affix(~side=L, tok), "")
          | (_, (Node(r), _)) when Token.merges(tok, r) =>
            Token.("", affix(~side=R, tok))
          | _ => assert(false)
          };
        P.log("--- delete_sel ---");
        P.show("z.ctx", Ctx.show(z.ctx));
        P.show("l", l);
        P.show("r", r);
        // let l =
        //   Cursor.is_point(cur) && sel.focus == L
        //     ? "" : Token.affix(~side=L, tok);
        // let r =
        //   Cursor.is_point(cur) && sel.focus == R
        //     ? "" : Token.affix(~side=R, tok);
        let tok = {...tok, text: l};
        let tok = {
          ...tok,
          marks:
            Token.is_complete(tok)
              ? None : Some(Point(Step.Caret.focus(Utf8.length(l)))),
        };
        let ctx =
          z.ctx
          |> Ctx.peel(~from=L, tok)
          |> Ctx.peel(~from=R, tok)
          |> Ctx.push(~onto=L, tok);
        (ctx, r);
      }
    | [_, _, ..._] as toks =>
      let (pre, ft) = Lists.Framed.ft_exn(toks);
      let (mid, hd) = Lists.Framed.ft_exn(pre);
      let ctx =
        z.ctx
        |> Ctx.peel(~from=L, hd)
        |> Ctx.peel(~from=R, ft)
        |> Ctx.push(~onto=L, {...hd, text: Token.affix(~side=L, hd)});
      let ctx =
        mid
        |> List.fold_left(
             (ctx, tok) => Ctx.push(~onto=L, {...tok, text: ""}, ctx),
             ctx,
           );
      (ctx, Token.affix(~side=R, ft));
    }
  };
};

// insert and delete_sel are mutually recursive because insert calls delete_sel to
// remove selection before inserting text, while delete_sel calls insert to
// reprocess the selection-external suffix of the rightmost selected token. the
// decreasing argument is the zipper's selection (nonempty to empty), with
// delete_sel doing nothing in the base case of an empty selection.
let insert = (s: string, z: Zipper.t) => {
  let (ctx, s') = delete_sel(z);
  let s = s ++ s';
  let z = Zipper.mk(ctx);
  open Options.Syntax;
  let- () = try_extend(s, z);
  let- () = try_expand(s, z);

  let (toks, r, ctx) = relabel(s, z.ctx);
  let (cell, ctx) =
    toks |> List.fold_left(mold, ctx) |> remold(~fill=Cell.point(Focus));
  Zipper.unzip_exn(cell, ~ctx) |> Move.hstep_n(- (Utf8.length(s') + r));
};

let delete = (d: Dir.t, z: Zipper.t) => {
  open Options.Syntax;
  P.log("--- delete ---");
  P.show("z", Zipper.show(z));
  let+ z = Cursor.is_point(z.cur) ? Select.hstep(d, z) : return(z);
  P.show("selected", Zipper.show(z));
  P.sexp("selected cursor", Zipper.Cursor.sexp_of_t(z.cur));
  let (ctx, s) = delete_sel(z);
  P.show("ctx", Ctx.show(ctx));
  P.show("s", s);
  let r = Zipper.mk(ctx) |> insert(s) |> Move.hstep_n(- Utf8.length(s));
  P.show("r", Zipper.show(r));
  r;
};
