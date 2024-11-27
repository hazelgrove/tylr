open Stds;

let relabel =
    (s: string, ctx: Ctx.t): (Chain.t(Cell.t, Token.Unmolded.t), Ctx.t) => {
  let (l, rest) =
    switch (Ctx.pull(~from=L, ctx)) {
    // hack to avoid merging usr/sys space tokens
    | (Node(tok), _) when Token.Space.is(tok) => (Delim.root, ctx)
    | (Node(tok), _) when tok.text == "" => (Delim.root, ctx)
    | (l, rest) => (l, rest)
    };
  let (r, rest) =
    switch (Ctx.pull(~from=R, rest)) {
    | (Node(tok), _) when Token.Space.is(tok) => (Delim.root, rest)
    | (Node(tok), _) when tok.text == "" => (Delim.root, rest)
    | (r, rest) => (r, rest)
    };
  let merges = Delim.merges(l, r);
  let s_l =
    Delim.is_tok(l)
    |> Option.map(Token.affix(~side=L, ~default_all=true))
    |> Option.value(~default="");
  let s_r =
    Delim.is_tok(r)
    |> Option.map(Token.affix(~side=R, ~default_all=true))
    |> Option.value(~default="");
  let labeled = Labeler.label(s_l ++ s ++ s_r);
  // push left face back if its labeling remains unchanged
  let (labeled, rest, pushed_back_l) =
    switch (labeled) {
    | [hd, ...tl] when hd.text == s_l && s_l != "" && !merges =>
      let ctx =
        Delim.is_tok(l)
        |> Option.map(t => Ctx.push(~onto=L, t, rest))
        |> Option.value(~default=rest);
      (tl, ctx, true);
    | labeled => (labeled, rest, false)
    };
  // push right face back if its labeling remains unchanged
  let (labeled, rest, pushed_back_r) =
    switch (Lists.Framed.ft(labeled)) {
    | Some((pre, ft)) when ft.text == s_r && s_r != "" && !merges =>
      let ctx =
        Delim.is_tok(r)
        |> Option.map(t => Ctx.push(~onto=R, t, rest))
        |> Option.value(~default=rest);
      // (List.rev(pre), 0, ctx);
      (List.rev(pre), ctx, true);
    | _ =>
      // (labeled, Utf8.length(s_r), rest)
      (labeled, rest, false)
    };

  // restore caret position
  let n = Utf8.length((pushed_back_l ? "" : s_l) ++ s);
  let (_, marked) =
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
       );
  // normalize the cursors by popping off any carets at the token edges
  // and storing them instead in neighboring cells, the final result being a
  // chain of cell-loops (either empty or with a caret) and token-links
  let normalized =
    marked
    |> Lists.fold_right(
         ~init=Chain.unit(Cell.dirty), ~f=(tok: Token.Unmolded.t, c) =>
         switch (tok.marks) {
         | Some(Point({path: 0, _})) =>
           Chain.link(
             Cell.point(~dirty=true, Focus),
             Token.clear_marks(tok),
             c,
           )
         | Some(Point({path: n, _})) when n == Utf8.length(tok.text) =>
           c
           |> Chain.map_hd(Fun.const(Cell.point(~dirty=true, Focus)))
           |> Chain.link(Cell.dirty, Token.clear_marks(tok))
         | _ => Chain.link(Cell.dirty, tok, c)
         }
       );
  // if both faces were pushed back, then use original ctx to preserve any closed
  // frames broken by pulling faces
  let ctx = pushed_back_l && pushed_back_r ? ctx : rest;
  (normalized, ctx);
};

// None means token was removed. Some(ctx) means token was molded (or deferred and
// tagged as an unmolded space), ctx includes the molded token.
let mold =
    (ctx: Ctx.t, ~fill=Cell.dirty, tok: Token.Unmolded.t)
    : Result.t(Ctx.t, Cell.t) => {
  open Result.Syntax;
  let ((l, r), rest) = Ctx.unlink_stacks(ctx);
  // Grouter.dbg := true;
  let+ (tok, grouted, l) = Molder.mold(l, ~fill, tok);
  // Grouter.dbg := false;
  // P.log("--- Molder.mold/success");
  // P.show("tok", Token.show(tok));
  // P.show("grouted", Grouted.show(grouted));
  // P.show("stack", Stack.show(l));
  let connected = Stack.connect(tok, grouted, l);
  // P.show("connected", Stack.show(connected));
  connected.bound == l.bound
    ? Ctx.link_stacks((connected, r), rest)
    : Ctx.map_hd(
        Frame.Open.cat(Stack.(to_slope(connected), to_slope(r))),
        rest,
      );
};

let rec remold = (~fill=Cell.dirty, ctx: Ctx.t): (Cell.t, Ctx.t) => {
  // P.log("--- Modify.remold");
  // P.show("fill", Cell.show(fill));
  // P.show("ctx", Ctx.show(ctx));
  let ((l, r), tl) = Ctx.unlink_stacks(ctx);
  switch (Molder.remold(~fill, (l, r))) {
  | Error((fill, (l', r'))) =>
    // remold error means something in r melded onto the bound of l, breaking their
    // bidelimited container, so we need to add the suffix of the next stack frame
    // in tl to the remolding queue
    tl
    |> Ctx.map_hd(Frame.Open.cat(Stack.(to_slope(l'), to_slope(r'))))
    |> remold(~fill)
  | Ok((dn, fill)) =>
    // P.log("--- Modify.remold/done");
    // P.show("dn", Slope.Dn.show(dn));
    // P.show("fill", Cell.show(fill));
    let bounds = (l.bound, r.bound);
    // Melder.dbg := true;
    let cell = Melder.complete_bounded(~bounds, ~onto=L, dn, ~fill);
    // Melder.dbg := false;
    // P.show("completed", Cell.show(cell));
    let hd = ({...l, slope: []}, {...r, slope: []});
    let ctx = Ctx.link_stacks(hd, tl);
    (cell, ctx);
  };
};

let finalize = (~mode=Mode.Navigating, ~fill=Cell.dirty, ctx: Ctx.t): Zipper.t => {
  Mode.set(mode);
  // P.log("--- finalize");
  let (remolded, ctx) = remold(~fill, ctx);
  // P.show("remolded", Cell.show(remolded));
  // P.show("ctx", Ctx.show(ctx));
  let (l, r) = Ctx.(face(~side=L, ctx), face(~side=R, ctx));
  let repadded = Linter.repad(~l, remolded, ~r);
  // P.show("repadded", Cell.show(repadded));
  let c = {...repadded, marks: Cell.Marks.flush(repadded.marks)};
  // P.show("flushed", Cell.show(c));
  Mode.reset();
  Zipper.unzip_exn(c, ~ctx);
};

let try_move = (s: string, z: Zipper.t) =>
  switch (s, Ctx.face(~side=R, z.ctx)) {
  | (" ", Node(tok))
      when
        String.starts_with(~prefix=" ", tok.text) || Mtrl.is_grout(tok.mtrl) =>
    Move.perform(Step(H(R)), z)
  | ("\n", Node(tok)) when String.starts_with(~prefix="\n", tok.text) =>
    Move.perform(Step(H(R)), z)
  | _ => None
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
  | Space(White(_))
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
  // todo: check if in middle of token
  let (face, rest) = Ctx.pull(~from=L, z.ctx);
  let* tok = Delim.is_tok(face);
  // if expandable, consider all expandable const labels
  let* expanded = expand(tok);
  let ((l, r), tl) = Ctx.unlink_stacks(rest);
  let* (t, grouted, rest) = Result.to_option(Molder.mold(l, expanded));
  if (t.mtrl == Space(Unmolded) || t.mtrl == tok.mtrl) {
    None;
  } else {
    let connected = Stack.connect(t, grouted, rest);
    tl
    |> (
      connected.bound == l.bound
        ? Ctx.link_stacks((connected, r))
        : Ctx.map_hd(
            Frame.Open.cat(Stack.(to_slope(connected), to_slope(r))),
          )
    )
    |> Ctx.push(~onto=L, Token.space())
    |> Ctx.trim_space(~side=R)
    |> finalize(~mode=Inserting(" "), ~fill=Cell.point(~dirty=true, Focus))
    |> return;
  };
};

let put_edge = (~hand=Caret.Hand.Focus, side: Dir.t, tok: Token.t) =>
  switch (side) {
  | L => Token.put_cursor(Point(Caret.mk(hand, 0)), tok)
  | R => Token.put_cursor(Point(Caret.mk(hand, Token.length(tok))), tok)
  };
let add_edge = (~hand=Caret.Hand.Focus, side: Dir.t, tok: Token.t) =>
  switch (side) {
  | L => Token.add_mark(Caret.mk(hand, 0), tok)
  | R => Token.add_mark(Caret.mk(hand, Token.length(tok)), tok)
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
         // (assuming edge carets have been temporarily non-normally placed on toks)
         let (l, r) = Token.(affix(~side=L, tok), affix(~side=R, tok));
         let text = Token.is_const(tok) ? l : l ++ r;
         {...tok, text}
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
       ~init=Chain.unit(Cell.dirty),
       ~f=(tok, c) => {
         let (l, tok, r) = Token.pop_end_carets(tok);
         c
         |> Chain.map_hd(r == None ? Fun.id : Fun.const(Cell.point(Focus)))
         |> (
           switch (tok.mtrl) {
           | Space(_)
           | Grout(_) when tok.text == "" =>
             Chain.map_hd(l == None ? Fun.id : Fun.const(Cell.point(Focus)))
           | _ => Chain.link(l == None ? Cell.dirty : Cell.point(Focus), tok)
           }
         );
       },
     )
  // finally, unmold the tokens (only relabeling the last token)
  |> Chain.mapi_link(i => Token.unmold(~relabel=i - 1 / 2 == n - 1));
};

// mold each token against the ctx, using each preceding cell as its fill, and
// return the total ctx and the final remaining fill to be used when subsequently
// remolding
let insert_toks =
    (toks: Chain.t(Cell.t, Token.Unmolded.t), ctx: Ctx.t): (Ctx.t, Cell.t) => {
  toks
  |> Chain.fold_left(
       fill => (ctx, fill),
       ((ctx, fill), tok, next_fill) => {
         //  P.log("--- insert_toks/tok");
         //  P.show("ctx", Ctx.show(ctx));
         //  P.show("fill", Cell.show(fill));
         //  P.show("tok", Token.Unmolded.show(tok));
         switch (mold(ctx, ~fill, tok)) {
         | Ok(ctx) =>
           //  P.show("molded tok", Ctx.show(ctx));
           let (face, rest) = Ctx.pull(~from=L, ctx);
           switch (face, next_fill.marks.cursor) {
           // if molded token is longer than original, then move cursor out of
           // next_fill and into molded token at the end of its text
           | (Node(molded), Some(Point({hand, path: []})))
               when Token.length(molded) > Token.Unmolded.length(tok) =>
             let marks = {...next_fill.marks, cursor: None};
             let next_fill = {...next_fill, marks};
             let molded =
               Token.put_cursor(
                 Point(Caret.mk(hand, Token.Unmolded.length(tok))),
                 molded,
               );
             let ctx = Ctx.push(~onto=L, molded, ~fill=Cell.dirty, rest);
             (ctx, next_fill);
           | _ => (ctx, next_fill)
           };
         | Error(fill) =>
           // removed empty token
           let next_fill =
             Cell.mark_ends_dirty(Cell.Space.merge(fill, next_fill));
           (ctx, next_fill);
         }
       },
     );
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
    let (sites, ctx) = Zipper.cursor_site(z);
    let (l, r) = Option.get(Cursor.get_select(sites));
    let deleted_toks =
      Zigg.tokens(sel.range)
      |> (
        l == Between
          ? Lists.map_hd(add_edge(~hand=sel.focus == L ? Focus : Anchor, L))
          : Fun.id
      )
      |> (
        r == Between
          ? Lists.map_ft(add_edge(~hand=sel.focus == R ? Focus : Anchor, R))
          : Fun.id
      )
      |> delete_toks(d);
    // P.log("--- delete_sel/Select");
    // P.show("ctx sans sites", Ctx.show(ctx));
    // P.show("site l", Zipper.Site.show(l));
    // P.show("site r", Zipper.Site.show(r));
    // P.show(
    //   "deleted_toks",
    //   Chain.show(Cell.pp, Token.Unmolded.pp, deleted_toks),
    // );
    let (molded, fill) = insert_toks(deleted_toks, ctx);
    // P.show("molded", Ctx.show(molded));
    // P.show("fill", Cell.show(fill));
    finalize(~mode=Deleting(d), ~fill, molded);
  };
};

let delete = (d: Dir.t, z: Zipper.t) => {
  open Options.Syntax;
  // let/ () =
  //   // first try moving over space tokens.
  //   // need to refine this re: usr vs sys.
  //   switch (Ctx.face(~side=d, z.ctx)) {
  //   | Node({mtrl: Space(White(Sys)), text, _})
  //       when
  //         Cursor.is_point(z.cur)
  //         && text
  //         |> Dir.pick(d, (Strings.rev, Fun.id))
  //         |> String.starts_with(~prefix=" ") =>
  //     Move.perform(Step(H(d)), z)
  //   | _ => None
  //   };
  // P.log("--- delete");
  // P.show("z", Zipper.show(z));
  let+ z =
    Cursor.is_point(z.cur) ? Select.hstep(~char=true, d, z) : return(z);
  // P.show("selected", Zipper.show(z));
  delete_sel(d, z);
};

let insert = (s: string, z: Zipper.t) => {
  open Options.Syntax;
  let z = delete_sel(L, z);

  // P.log("--- Modify.insert");
  let- () = try_expand(s, z);
  let- () = try_move(s, z);
  let- () = try_extend(s, z);

  // P.log("--- Modify.insert/molding");
  // P.show("z.ctx", Ctx.show(z.ctx));
  let (toks, ctx) = relabel(s, z.ctx);
  // P.show("toks", Chain.show(Cell.pp, Token.Unmolded.pp, toks));
  // P.show("ctx", Ctx.show(ctx));
  let (molded, fill) = insert_toks(toks, ctx);
  // P.show("molded", Ctx.show(molded));
  // P.show("fill", Cell.show(fill));
  finalize(~mode=Inserting(s), ~fill, molded);
};
