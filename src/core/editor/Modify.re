open Stds;

type to_be_inserted = Chain.t(Cell.t, Token.Unmolded.t);

let restore_and_normalize_cursor =
    (n: int, toks: list(Token.Unmolded.t)): to_be_inserted => {
  // restore caret position
  let (_, marked) =
    toks
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
};

let relabel = (s: string, z: Zipper.t): list((to_be_inserted, Ctx.t)) => {
  let (cur_site, ctx) = Zipper.cursor_site(z);
  switch (Option.get(Cursor.get_point(cur_site))) {
  | Within(tok) =>
    let (l, r) = Token.(affix(~side=L, tok), affix(~side=R, tok));
    let labeled = Labeler.label(l ++ s ++ r);
    let n = Utf8.length(l ++ s);
    let toks = restore_and_normalize_cursor(n, labeled);
    [(toks, ctx)];
  | Between =>
    let (l, ctx_sans_l) = Ctx.pull(~from=L, ctx);
    let (r, ctx_sans_r) = Ctx.pull(~from=R, ctx);
    let (_, ctx_sans_lr) = Ctx.pull(~from=R, ctx_sans_l);
    let no_merge = {
      let labeled = Labeler.label(s);
      let n = Utf8.length(s);
      let toks = restore_and_normalize_cursor(n, labeled);
      [(toks, ctx)];
    };
    let merged_l =
      switch (l) {
      | Root
      | Node({mtrl: Space(White(_)) | Grout(_), _}) => []
      | Node({mtrl: Space(Unmolded) | Tile(_), text: l, _}) =>
        switch (Labeler.single(l ++ s)) {
        | None => []
        | Some(tok) =>
          let n = Utf8.length(l ++ s);
          let toks = restore_and_normalize_cursor(n, [tok]);
          [(toks, ctx_sans_l)];
        }
      };
    let merged_r =
      switch (r) {
      | Root
      | Node({mtrl: Space(White(_)) | Grout(_), _}) => []
      | Node({mtrl: Space(Unmolded) | Tile(_), text: r, _}) =>
        switch (Labeler.single(s ++ r)) {
        | None => []
        | Some(tok) =>
          let n = Utf8.length(s);
          let toks = restore_and_normalize_cursor(n, [tok]);
          [(toks, ctx_sans_r)];
        }
      };
    let merged_lr =
      switch (l, r) {
      | (Root | Node({mtrl: Space(White(_)) | Grout(_), _}), _)
      | (_, Root | Node({mtrl: Space(White(_)) | Grout(_), _})) => []
      | (
          Node({mtrl: Space(Unmolded) | Tile(_), text: l, _}),
          Node({mtrl: Space(Unmolded) | Tile(_), text: r, _}),
        ) =>
        switch (Labeler.single(l ++ s ++ r)) {
        | None => []
        | Some(tok) =>
          let n = Utf8.length(l ++ s);
          let toks = restore_and_normalize_cursor(n, [tok]);
          [(toks, ctx_sans_lr)];
        }
      };
    List.concat([merged_lr, merged_l, merged_r, no_merge]);
  };
};

// None means token was removed. Some(ctx) means token was molded (or deferred and
// tagged as an unmolded space), ctx includes the molded token.
let mold =
    (ctx: Ctx.t, ~fill=Cell.dirty, tok: Token.Unmolded.t)
    : Result.t(Ctx.t, Cell.t) => {
  open Result.Syntax;
  // P.log("--- Modify.mold");
  // P.show("ctx", Ctx.show(ctx));
  // P.show("fill", Cell.show(fill));
  // P.show("tok", Token.Unmolded.show(tok));
  let ((l, r), rest) = Ctx.unlink_stacks(ctx);
  // Grouter.dbg := true;
  let+ (tok, grouted, l) = Molder.mold(l, ~fill, tok);
  // Grouter.dbg := false;
  // P.log("--- Modify.mold/success");
  // P.show("tok", Token.show(tok));
  // P.show("grouted", Grouted.show(grouted));
  // P.show("stack", Stack.show(l));
  let connected = Stack.connect(Effects.insert(tok), grouted, l);
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
    // P.log("--- Modify.remold/error");
    // P.show("fill", Cell.show(fill));
    // P.show("(l', r')", Stack.Frame.show((l', r')));
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
  // P.log("--- Modify.finalize");
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
let finalize_ = (remolded: Cell.t, ctx: Ctx.t): Zipper.t => {
  // P.log("--- Modify.finalize_");
  // P.show("remolded", Cell.show(remolded));
  // P.show("ctx", Ctx.show(ctx));
  let (l, r) = Ctx.(face(~side=L, ctx), face(~side=R, ctx));
  let repadded = Linter.repad(~l, remolded, ~r);
  // P.show("repadded", Cell.show(repadded));
  let c = {...repadded, marks: Cell.Marks.flush(repadded.marks)};
  // P.show("flushed", Cell.show(c));
  Zipper.unzip_exn(c, ~ctx);
};

let try_move = (s: string, z: Zipper.t) => {
  let (face, ctx) = Ctx.pull(~from=R, z.ctx);
  switch (s, face, Ctx.face(~side=R, ctx)) {
  | (" ", Node(tok), _) when tok.text == " " || Mtrl.is_grout(tok.mtrl) =>
    Move.perform(Step(H(R)), z)
  | ("\n", Node(tok), Node(next)) when tok.text == "\n" && next.text == "" =>
    Move.perform(Step(H(R)), z)
  | _ => None
  };
};

let extend = (~side=Dir.R, s: string, tok: Token.t) =>
  switch (tok.mtrl) {
  | Space(Unmolded) =>
    // this path may need extra guards, currently always succeeds at extending
    // unmolded token
    let (l, r) =
      Token.split_text(tok)
      |> Option.map(((l, _, r)) => (l, r))
      |> Option.value(
           ~default=Dir.pick(side, (("", tok.text), (tok.text, ""))),
         );
    let text = l ++ s ++ r;
    Labeler.single(text) |> Option.map(_ => {...tok, text});
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
let expand = (tok: Token.t): option(Token.Unmolded.t) =>
  switch (tok.mtrl) {
  | Space(White(_))
  | Grout(_) => None
  // | Tile((Const(_), _)) => None
  | Space(Unmolded) =>
    open Options.Syntax;
    let* labeled = Labeler.single(tok.text);
    // P.log("--- Modify.expand");
    // P.sexp("labeled", Token.Unmolded.sexp_of_t(labeled));
    switch (labeled.mtrl) {
    | Space(_)
    | Grout(_) => None
    | Tile(_) => Some(labeled)
    };
  | Tile(_) =>
    open Options.Syntax;
    let* labeled = Labeler.single(tok.text);
    Token.Unmolded.expands(labeled);
  };
let try_expand = (s: string, z: Zipper.t): option(Zipper.t) => {
  open Options.Syntax;
  // P.log("--- Modify.try_expand");
  let* () = Options.of_bool(String.starts_with(~prefix=" ", s));
  // todo: check if in middle of token
  let (face, rest) = Ctx.pull(~from=L, z.ctx);
  let* tok = Delim.is_tok(face);
  // if expandable, consider all expandable const labels
  let* expanded = expand(tok);
  let ((l, r), tl) = Ctx.unlink_stacks(rest);
  let* (t, grouted, rest) = Result.to_option(Molder.mold(l, expanded));
  // P.show("molded", Token.show(t));
  // P.show("grouted", Grouted.show(grouted));
  // P.show("stack", Stack.show(rest));
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

let meld_remold =
    (prev, tok: Token.t, next, ctx: Ctx.t): option((Cell.t, Ctx.t)) => {
  open Options.Syntax;
  let ((l, r), rest) = Ctx.unlink_stacks(ctx);
  let* (grouted, l) =
    Melder.push(tok, ~fill=prev, l, ~onto=L, ~repair=Molder.remold);
  let is_redundant =
    Token.is_empty(tok)
    && (
      Grouted.is_neq(grouted)
      || Option.is_some(Grouted.is_eq(grouted))
      && l.slope == []
    );
  if (is_redundant) {
    Effects.remove(tok);
    let fill = Cell.Space.merge(prev, next);
    Some(remold(~fill, ctx));
  } else {
    let connected = Stack.connect(Effects.insert(tok), grouted, l);
    let ctx =
      connected.bound == l.bound
        ? Ctx.link_stacks((connected, r), rest)
        : Ctx.map_hd(
            Frame.Open.cat(Stack.(to_slope(connected), to_slope(r))),
            rest,
          );
    let remolded = remold(~fill=next, ctx);
    // P.log("--- meld_remold");
    // P.show("tok", Token.show(tok));
    // P.show("ctx", Ctx.show(ctx));
    // P.show("remolded", Cell.show(fst(remolded)));
    // P.show("remolded ctx", Ctx.show(snd(remolded)));
    // P.show("effects", Fmt.(to_to_string(list(Effects.pp), Effects.log^)));
    switch (tok.mtrl) {
    | Tile((lbl, _))
        when
          !Label.is_instant(lbl)
          && Oblig.Delta.(not_hole(of_effects(Effects.log^))) =>
      None
    | _ => Some(remolded)
    };
  };
};

let candidates = (t: Token.Unmolded.t): list(Token.t) =>
  Molder.candidates(t) @ [Token.Unmolded.defer(t)];

let mold_remold =
    (prev, tok: Token.Unmolded.t, next, ctx: Ctx.t): (Cell.t, Ctx.t) => {
  candidates(tok)
  |> Oblig.Delta.minimize(
       //  ~show_y=
       //    ((cell, ctx)) => {
       //      P.show("cell", Cell.show(cell));
       //      P.show("ctx", Ctx.show(ctx));
       //    },
       tok =>
       meld_remold(prev, tok, next, ctx)
     )
  |> Options.get_fail(
       "bug: at least deferred candidate should have succeeded",
     );
};

let insert_remold =
    (toks: Chain.t(Cell.t, Token.Unmolded.t), ctx: Ctx.t): (Cell.t, Ctx.t) => {
  switch (Chain.(unlink(rev(toks)))) {
  | Error(cell) => remold(~fill=cell, ctx)
  | Ok((next, tok, toks)) =>
    let (ctx, prev) = insert_toks(Chain.rev(toks), ctx);
    mold_remold(prev, tok, next, ctx);
  };
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
    let (remolded, ctx) = insert_remold(deleted_toks, ctx);
    // P.show("molded", Ctx.show(molded));
    // P.show("fill", Cell.show(fill));
    finalize_(remolded, ctx);
  };
};

let try_truncate = (z: Zipper.t) => {
  switch (z.cur) {
  | Point(_) => None
  | Select(sel) =>
    // prune ctx of any duplicated tokens
    let (sites, ctx) = Zipper.cursor_site(z);
    let (l, r) = Option.get(Cursor.get_select(sites));
    // temporarily denormalize cursor for more convenient processing
    switch (
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
    ) {
    | [tok] =>
      switch (Token.split_text(tok)) {
      | Some((l, _, "")) when !Strings.is_empty(l) =>
        let tok = {
          ...tok,
          text: l,
          // renormalize cursor
          marks: Some(Point(Caret.focus(Utf8.length(l)))),
        };
        ctx
        |> Ctx.push(~onto=L, tok)
        |> (Token.is_complete(tok) ? Fun.id : Ctx.push(~onto=R, tok))
        |> Zipper.mk
        |> Option.some;
      | _ => None
      }
    | _ => None
    };
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
  let- () = try_truncate(z);
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

  let (remolded, ctx) =
    relabel(s, z)
    |> Oblig.Delta.minimize(((toks, ctx)) =>
         Some(insert_remold(toks, ctx))
       )
    |> Option.get;
  finalize_(remolded, ctx);
};
