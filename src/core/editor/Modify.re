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
      | Node({mtrl: Space(White(_)) | Grout(_), _})
      | Node({mtrl: Tile(_), text: "", _}) => []
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
      | Node({mtrl: Space(White(_)) | Grout(_), _})
      | Node({mtrl: Tile(_), text: "", _}) => []
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

let rec remold = (~fill=Cell.dirty, ctx: Ctx.t): (Grouted.t, Ctx.t) => {
  // P.log("--- Modify.remold");
  // P.show("fill", Cell.show(fill));
  // P.show("ctx", Ctx.show(ctx));
  let ((l, r), tl) = Ctx.unlink_stacks(ctx);
  switch (Molder.remold(~fill, (l, r))) {
  // ( 1 + 2 ) + 3 ) + 4
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
  // # 1 + ( <> >)> #
  | Ok((dn, fill)) =>
    // P.log("--- Modify.remold/done");
    // P.show("dn", Slope.Dn.show(dn));
    // P.show("fill", Cell.show(fill));
    let bounds = (l.bound, r.bound);
    // Melder.debug := true;
    let grouted = Melder.complete_bounded(~bounds, ~onto=L, dn, ~fill);
    // Melder.debug := false;
    // P.show("completed", Cell.show(cell));
    let hd = ({...l, slope: []}, {...r, slope: []});
    let ctx = Ctx.link_stacks(hd, tl);
    (grouted, ctx);
  };
};

let finalize_ = (remolded: Grouted.t, ctx: Ctx.t): Zipper.t => {
  // P.log("--- Modify.finalize_");
  // P.show("remolded", Grouted.show(remolded));
  // P.show("ctx", Ctx.show(ctx));
  let repadded =
    remolded
    |> Chain.rev
    |> Chain.map_link(Delim.tok)
    |> Chain.consnoc(
         ~hd=Ctx.face(~side=L, ctx),
         ~ft=Ctx.face(~side=R, ctx),
       )
    |> Chain.map_linked((l, (sw, c), r) => {
         let repadded = Linter.repad(~l, c, ~r);
         (sw, {...repadded, marks: Cell.Marks.flush(repadded.marks)});
       })
    |> Chain.unconsnoc_exn
    |> (((_, c, _)) => c)
    |> Chain.map_link(Delim.unwrap);
  // P.show("repadded", Grouted.show(repadded));
  let (pre, (_, cur), suf) =
    repadded
    |> Chain.find_unzip_loop(((_, c: Cell.t)) =>
         Option.is_some(c.marks.cursor)
       )
    |> Options.get_fail("bug: lost cursor");
  let ((l, r), rest) = Ctx.unlink_stacks(ctx);
  let l = Stack.connect_affix(pre, l);
  let r = Stack.connect_affix(suf, r);
  let ctx = Ctx.link_stacks((l, r), rest);
  // P.show("flushed", Cell.show(c));
  Zipper.unzip_exn(cur, ~ctx);
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
    let n = Utf8.length(l ++ s);
    Labeler.single(text)
    |> Option.map(_ => {...tok, text})
    |> Option.map(tok =>
         n >= Token.length(tok)
           ? Token.clear_marks(tok)
           : Token.put_cursor(Point(Caret.focus(n)), tok)
       );
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
  // P.log("--- Modify.try_extend");
  let* () = Options.of_bool(!Strings.is_empty(s));
  // P.log("not empty");
  let (sites, ctx) = Zipper.cursor_site(z);
  let* site = Cursor.get_point(sites);
  // P.log("cursor site is point");
  let+ (extended, ctx) =
    switch (site) {
    | Within(tok) =>
      let+ extended = extend(~side=R, s, tok);
      (extended, ctx);
    | Between =>
      let/ () = {
        // P.log("--- Modify.try_extend/Between/trying left");
        let (face, ctx) = Ctx.pull(~from=L, ctx);
        let* tok = Delim.is_tok(face);
        // P.log("is tok");
        let+ extended = extend(~side=R, s, tok);
        // P.log("extended");
        (extended, ctx);
      };
      // P.log("--- Modify.try_extend/Between/trying right");
      let (face, ctx) = Ctx.pull(~from=R, ctx);
      let* tok = Delim.is_tok(face);
      // P.log("is tok");
      let+ extended = extend(~side=L, s, tok);
      // P.log("extended");
      (extended, ctx);
    };
  ctx
  |> Ctx.push(~onto=L, extended)
  |> (Option.is_some(extended.marks) ? Ctx.push(~onto=R, extended) : Fun.id)
  |> Zipper.mk;
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
  let car = Cell.point(~dirty=true, Focus);
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
         |> Chain.map_hd(r == None ? Fun.id : Fun.const(car))
         |> (
           switch (tok.mtrl) {
           | Space(_)
           | Grout(_) when tok.text == "" =>
             Chain.map_hd(
               l == None
                 ? Fun.id
                 : car
                   |> Cell.map_marks(Cell.Marks.mark_degrouted([]))
                   |> Fun.const,
             )
           | _ => Chain.link(l == None ? Cell.dirty : car, tok)
           }
         );
       },
     )
  // finally, unmold the tokens (only relabeling the last token)
  |> Chain.mapi_link(i => Labeler.unmold(~relabel=i - 1 / 2 == n - 1));
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
         //  P.log("--- Modify.insert_toks/tok");
         //  P.show("tok", Token.Unmolded.show(tok));
         //  P.show("fill", Cell.show(fill));
         //  P.show("ctx", Ctx.show(ctx));
         switch (mold(ctx, ~fill, tok)) {
         | Ok(ctx) =>
           //  P.show("-- Modify.insert_toks/tok/Ok ctx", Ctx.show(ctx));
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
           //  P.log("--- Modify.insert_toks/tok/Error removed");
           // removed empty token
           let next_fill =
             Cell.mark_ends_dirty(Cell.Space.merge(fill, next_fill))
             |> Cell.mark_end_ungrouted(~side=R);
           (ctx, next_fill);
         }
       },
     );
};

let meld_remold =
    (~expanding=false, prev, tok: Token.t, next, ctx: Ctx.t)
    : option((Grouted.t, Ctx.t)) => {
  open Options.Syntax;
  // P.log("--- Modify.meld_remold");
  // P.show("prev", Cell.show(prev));
  // P.sexp("tok", Token.sexp_of_t(tok));
  // P.show("next", Cell.show(next));
  // P.show("ctx", Ctx.show(ctx));
  let ((l, r), rest) = Ctx.unlink_stacks(ctx);
  let* (grouted, l) =
    Melder.push(tok, ~fill=prev, l, ~onto=L, ~repair=Molder.remold);
  // P.log("--- Modify.meld_remold/pushed");
  // P.show("grouted", Grouted.show(grouted));
  // P.show("l", Stack.show(l));
  let is_redundant =
    tok.text == ""
    && (
      Token.is_complete(tok)
      || Grouted.is_neq(grouted)
      || Option.is_some(Grouted.is_eq(grouted))
      && l.slope == []
    );
  if (is_redundant) {
    // P.log("--- Modify.meld_remold/is_redundant");
    Effects.remove(tok);
    let fill = Cell.Space.merge(prev, ~fill=Cell.degrouted, next);
    // P.log("--- Modify.meld_remold/redundant/remolding");
    Some(remold(~fill, ctx));
  } else {
    // P.log("--- Modify.meld_remold/not_redundant");
    let connected = Stack.connect(Effects.insert(tok), grouted, l);
    let ctx =
      connected.bound == l.bound
        ? Ctx.link_stacks((connected, r), rest)
        : Ctx.map_hd(
            Frame.Open.cat(Stack.(to_slope(connected), to_slope(r))),
            rest,
          );
    // todo: generalize this to other expansion triggers
    let ctx =
      expanding
        ? ctx |> Ctx.push(~onto=L, Token.space()) |> Ctx.trim_space(~side=R)
        : ctx;
    // P.log("--- Modify.meld_remold/not_redundant/remolding");
    let remolded = remold(~fill=next, ctx);
    // P.log("--- meld_remold/not redundant");
    // P.show("tok", Token.show(tok));
    // P.show("ctx", Ctx.show(ctx));
    // P.show("remolded", Grouted.show(fst(remolded)));
    // P.show("remolded ctx", Ctx.show(snd(remolded)));
    // P.show("effects", Fmt.(to_to_string(list(Effects.pp), Effects.log^)));
    switch (tok.mtrl) {
    | Tile((lbl, _))
        // delay expansion if obligations more severe than holes
        when
          !expanding
          && !Label.is_instant(lbl)
          && Oblig.Delta.(not_hole(of_effects(Effects.log^)))
          // this is necessary when deleting delims to empty ghosts
          && !Token.is_empty(tok)
          // only delay expansion for tokens followed by caret (#126)
          && Option.(is_some(tok.marks) || is_some(next.marks.cursor)) =>
      None
    | _ => Some(remolded)
    };
  };
};

let expand_remold =
    (tok: Token.Unmolded.t, ~fill, ctx: Ctx.t)
    : (Token.t, (Grouted.t, Ctx.t)) => {
  switch (
    Molder.candidates(tok)
    |> Oblig.Delta.minimize(tok =>
         meld_remold(~expanding=true, Cell.dirty, tok, fill, ctx)
         |> Option.map(r => (tok, r))
       )
  ) {
  | Some(r) => r
  | None =>
    let tok = Token.Unmolded.defer(tok);
    let r =
      meld_remold(~expanding=true, Cell.dirty, tok, fill, ctx)
      |> Options.get_fail(
           "bug: at least deferred candidate should have succeeded",
         );
    (tok, r);
  };
};
// maybe rename expandable
let expand = (tok: Token.t): option(Token.Unmolded.t) =>
  switch (tok.mtrl) {
  | Space(White(_))
  | Grout(_) => None
  | Space(Unmolded) =>
    open Options.Syntax;
    let* labeled = Labeler.single(tok.text);
    switch (labeled.mtrl) {
    | Space(_)
    | Grout(_) => None
    | Tile(_) => Some(labeled)
    };
  | Tile(_) =>
    open Options.Syntax;
    let* labeled = Labeler.single(~id=tok.id, tok.text);
    Token.Unmolded.expands(labeled);
  };
let try_expand = (s: string, z: Zipper.t): option(Zipper.t) => {
  open Options.Syntax;
  let effects = Effects.log^;
  let* () = Options.of_bool(String.starts_with(~prefix=" ", s));
  // todo: check if in middle of token
  let (face, rest) = Ctx.pull(~from=L, z.ctx);
  let* tok = Delim.is_tok(face);
  // if expandable, consider all expandable const labels
  let* expanded = expand(tok);
  let (molded, (remolded, ctx)) =
    expand_remold(expanded, ~fill=Cell.point(~dirty=true, Focus), rest);
  molded == tok
    ? {
      Effects.log := effects;
      None;
    }
    : return(finalize_(remolded, ctx));
};

let mold_remold =
    (prev, tok: Token.Unmolded.t, next, ctx: Ctx.t): (Grouted.t, Ctx.t) => {
  open Options.Syntax;
  // P.log("--- Modify.mold_remold");
  // P.show("prev", Cell.show(prev));
  // P.show("tok", Token.Unmolded.show(tok));
  // P.show("next", Cell.show(next));
  // P.show("ctx", Ctx.show(ctx));
  let- () =
    Molder.candidates(tok)
    @ (tok.text == "" ? [] : [Token.Unmolded.defer(tok)])
    |> Oblig.Delta.minimize(tok => {meld_remold(prev, tok, next, ctx)});
  assert(tok.text == "");
  let fill = Cell.Space.merge(prev, ~fill=Cell.degrouted, next);
  remold(~fill, ctx);
};

let insert_remold =
    (toks: Chain.t(Cell.t, Token.Unmolded.t), ctx: Ctx.t)
    : (Grouted.t, Ctx.t) => {
  // P.log("--- Modify.insert_remold");
  // P.show("ctx", Ctx.show(ctx));
  switch (Chain.(unlink(rev(toks)))) {
  | Error(cell) => remold(~fill=cell, ctx)
  | Ok((next, tok, toks)) =>
    // P.log("--- Modify.insert_remold/Ok");
    // P.show("next", Cell.show(next));
    // P.show("tok", Token.Unmolded.show(tok));
    // P.show("toks", Chain.show(Cell.pp, Token.Unmolded.pp, toks));
    let (ctx, prev) = insert_toks(Chain.rev(toks), ctx);
    // P.show("inserted toks ctx", Ctx.show(ctx));
    // P.show("prev", Cell.show(prev));
    mold_remold(prev, tok, next, ctx);
  };
};

// delete_sel clears the textual content of the current selection (doing nothing if
// the selection is empty). this entails dropping all of the zigg's cells and
// remelding the zigg's tokens as empty ghosts onto (the left side of) the ctx. in
// the case of tokens at the ends of the selection that are split by the selection
// boundaries, the selection-external affixes of those tokens are preserved.
let delete_sel = (d: Dir.t, z: Zipper.t): Zipper.t => {
  // P.log("--- Modify.delete_sel");
  Mode.set(Deleting(d));
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
    // P.log("--- Modify.delete_sel/Select");
    // P.show("ctx sans sites", Ctx.show(ctx));
    // P.show("site l", Zipper.Site.show(l));
    // P.show("site r", Zipper.Site.show(r));
    // P.show(
    //   "deleted_toks",
    //   Chain.show(Cell.pp, Token.Unmolded.pp, deleted_toks),
    // );
    let (remolded, ctx) = insert_remold(deleted_toks, ctx);
    finalize_(remolded, ctx);
  };
};

let try_truncate = (z: Zipper.t) => {
  switch (z.cur) {
  | Point(_) => None
  | Select(sel) =>
    // P.log("--- Modify.try_truncate");
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
        // P.log("--- Modify.try_truncate/success");
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
  // P.log("--- Modify.delete");
  // P.show("z", Zipper.show(z));
  Mode.set(Deleting(d));
  let+ z =
    Cursor.is_point(z.cur) ? Select.hstep(~char=true, d, z) : return(z);
  let- () = try_truncate(z);
  // P.log("didn't truncate");
  // P.show("selected", Zipper.show(z));
  delete_sel(d, z);
};

let insert = (s: string, z: Zipper.t) => {
  open Options.Syntax;
  // P.log("--- Modify.insert");
  let z = delete_sel(L, z);
  // P.show("deleted", Zipper.show(z));

  Mode.set(Inserting(s));
  // P.log("--- Modify.insert");
  let- () = try_expand(s, z);
  // P.log("--- Modify.insert/didn't expand");
  let- () = try_move(s, z);
  // P.log("--- Modify.insert/didn't move");
  let- () = try_extend(s, z);
  // P.log("--- Modify.insert/didn't extend");

  let (remolded, ctx) =
    relabel(s, z)
    |> Oblig.Delta.minimize(((toks, ctx)) => {
         // P.show("toks", Chain.show(Cell.pp, Token.Unmolded.pp, toks));
         // P.show("ctx", Ctx.show(ctx));
         Some(
           insert_remold(toks, ctx),
         )
       })
    |> Option.get;
  // P.show("remolded", Cell.show(remolded));
  // P.show("ctx", Ctx.show(ctx));

  finalize_(remolded, ctx);
};
