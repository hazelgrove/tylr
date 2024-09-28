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

// returns token with updated carets after movement, where carets that reach the
// token edges are removed. returned token is accompanied by flag indicating whether
// the moved caret reached the token edge.
let hstep_tok = (d: Dir.t, tok: Token.t): (Token.t, bool) => {
  let (m, n) = (Token.length(tok), Utf8.length(tok.text));
  let (l, r) = (1, Token.is_complete(tok) ? m - 1 : n);
  switch (tok.marks) {
  | _ when m <= 1 || n <= 0 => (Token.clear_marks(tok), true)
  | None =>
    let car = Caret.focus(Dir.pick(d, (r, l)));
    (Token.put_cursor(Point(car), tok), false);
  | Some(Point({hand: Anchor, _} as anc)) =>
    let foc = Caret.focus(Dir.pick(d, (r, l)));
    let cur = Step.Cursor.mk(foc, anc);
    (Token.put_cursor(cur, tok), false);
  | Some(Point({hand: Focus, _} as foc)) =>
    if (Dir.pick(d, (foc.path <= l, foc.path >= r))) {
      (Token.clear_marks(tok), true);
    } else {
      let foc = Step.Caret.shift(Dir.pick(d, ((-1), 1)), foc);
      (Token.put_cursor(Point(foc), tok), false);
    }
  | Some(Select(sel)) =>
    let (foc, anc) = Dir.order(sel.focus, Step.Selection.carets(sel));
    if (Dir.pick(d, (foc.path <= l, foc.path >= r))) {
      (Token.put_cursor(Point(anc), tok), true);
    } else {
      let foc = Step.Caret.shift(Dir.pick(d, ((-1), 1)), foc);
      let cur = Step.Cursor.mk(foc, anc);
      (Token.put_cursor(cur, tok), false);
    };
  };
};

let push_site = (~onto: Dir.t, site: Zipper.Site.t, ctx: Ctx.t) =>
  switch (site) {
  | Between => ctx
  | Within(tok) => Ctx.push(~onto, tok, ctx)
  };

let hstep = (d: Dir.t, z: Zipper.t): option(Zipper.t) => {
  open Options.Syntax;
  // let b = Dir.toggle(d);
  let (cur_site, ctx_sans_site) = Zipper.cursor_site(z);
  let growing =
    switch (z.cur) {
    | Point(_) => true
    | Select({focus, _}) => focus == d
    };
  if (growing) {
    let (delim, ctx_sans_delim) =
      switch (cur_site) {
      | Point(Between) => Ctx.pull(~from=d, ctx_sans_site)
      | Point(Within(tok)) => (Node(tok), ctx_sans_site)
      | Select(sites) =>
        switch (Dir.pick(d, sites)) {
        | Between => Ctx.pull(~from=d, ctx_sans_site)
        | Within(tok) => (Node(tok), ctx_sans_site)
        }
      };
    let+ tok = Delim.is_tok(delim);
    let (stepped, exited) = hstep_tok(d, tok);
    let zigg =
      switch (z.cur) {
      | Point(_) => Zigg.of_tok(stepped)
      | Select(sel) => Zigg.grow(~side=sel.focus, stepped, sel.range)
      };
    let sel = Selection.{focus: d, range: zigg};
    let ctx =
      exited ? ctx_sans_delim : Ctx.push(~onto=d, stepped, ctx_sans_delim);
    Zipper.mk(~cur=Select(sel), ctx);
  } else {
    // points always grow, only selections can shrink.
    // d points toward selection anchor.
    let sel = Option.get(Cursor.get_select(z.cur));
    let sites = Option.get(Cursor.get_select(cur_site));
    let (_site_foc, site_anc) = Dir.order(sel.focus, sites);

    let (tok, rest) = Zigg.pull(~side=sel.focus, sel.range);
    let (stepped, exited) = hstep_tok(d, tok);

    switch (rest) {
    | Some(rest) =>
      // sel spanned more than one token
      let zigg = exited ? rest : Zigg.grow(~side=sel.focus, stepped, rest);
      let cur = Cursor.Select({...sel, range: zigg});
      let ctx =
        ctx_sans_site
        |> Ctx.push(~onto=sel.focus, stepped)
        |> push_site(~onto=Dir.toggle(sel.focus), site_anc);
      return(Zipper.mk_button(~cur, ctx));
    | None =>
      // sel was a single token, need to consider anchor side more carefully
      switch (stepped.marks, site_anc) {
      | (None, _) =>
        // must be true bc anchor does not move
        assert(site_anc == Between);
        let cur = Cursor.Point(Caret.focus());
        let ctx = Ctx.push(~onto=sel.focus, stepped, ctx_sans_site);
        return(Zipper.mk_button(~cur, ctx));
      | (Some(Point(car)), Between) =>
        assert(car.hand == Focus);
        let cur = Cursor.Select({...sel, range: Zigg.of_tok(stepped)});
        let ctx = Ctx.push(~onto=sel.focus, stepped, ctx_sans_site);
        return(Zipper.mk_button(~cur, ctx));
      | (Some(Point(car)), Within(_)) =>
        assert(car.hand == Focus);
        let cur = Cursor.Point(Caret.focus());
        let ctx =
          // maybe can encapsulate as push point ctx
          ctx_sans_site
          |> Ctx.push(~onto=sel.focus, stepped)
          |> Ctx.push(~onto=Dir.toggle(sel.focus), stepped);
        return(Zipper.mk_button(~cur, ctx));
      | (Some(Select(_)), _) =>
        let cur = Cursor.Select({...sel, range: Zigg.of_tok(stepped)});
        let ctx =
          ctx_sans_site
          |> Ctx.push(~onto=sel.focus, stepped)
          |> Ctx.push(~onto=Dir.toggle(sel.focus), stepped);
        return(Zipper.mk_button(~cur, ctx));
      }
    };
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
