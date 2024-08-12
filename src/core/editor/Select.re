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

// returns token with updated carets after movement, where carets at the token edges
// are removed. returned token is accompanied by flag indicating whether the moved
// caret reached the token edge.
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
      | Point(Between) => Ctx.pull_face(~from=d, ctx_sans_site)
      | Point(Within(tok)) => (Node(tok), ctx_sans_site)
      | Select(sites) =>
        switch (Dir.pick(d, sites)) {
        | Between => Ctx.pull_face(~from=d, ctx_sans_site)
        | Within(tok) => (Node(tok), ctx_sans_site)
        }
      };
    let+ tok = Delim.is_tok(delim);
    let (stepped, exited) = hstep_tok(d, tok);
    let sel = Selection.{focus: d, range: Zigg.of_tok(stepped)};
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
      return(Zipper.mk(~cur, ctx));
    | None =>
      // sel was a single token, need to consider anchor side more carefully
      switch (stepped.marks, site_anc) {
      | (None, _) =>
        // must be true bc anchor does not move
        assert(site_anc == Between);
        let cur = Cursor.Point(Caret.focus());
        let ctx = Ctx.push(~onto=sel.focus, stepped, ctx_sans_site);
        return(Zipper.mk(~cur, ctx));
      | (Some(Point(car)), Between) =>
        assert(car.hand == Focus);
        let cur = Cursor.Select({...sel, range: Zigg.of_tok(stepped)});
        let ctx = Ctx.push(~onto=sel.focus, stepped, ctx_sans_site);
        return(Zipper.mk(~cur, ctx));
      | (Some(Point(car)), Within(_)) =>
        assert(car.hand == Focus);
        let cur = Cursor.Point(Caret.focus());
        let ctx =
          // maybe can encapsulate as push point ctx
          ctx_sans_site
          |> Ctx.push(~onto=sel.focus, stepped)
          |> Ctx.push(~onto=Dir.toggle(sel.focus), stepped);
        return(Zipper.mk(~cur, ctx));
      | (Some(Select(_)), _) =>
        let cur = Cursor.Select({...sel, range: Zigg.of_tok(stepped)});
        let ctx =
          ctx_sans_site
          |> Ctx.push(~onto=sel.focus, stepped)
          |> Ctx.push(~onto=Dir.toggle(sel.focus), stepped);
        return(Zipper.mk(~cur, ctx));
      }
    };
  };
};

// let hstep = (d: Dir.t, z: Zipper.t): option(Zipper.t) => {
//   open Options.Syntax;
//   let b = Dir.toggle(d);
//   // note: this pulls any site tokens from ctx
//   let (site, ctx) = Zipper.cursor_site(z);
//   switch (site) {
//   | Point(Between) =>
//     let (delim, ctx) = Ctx.pull_face(~from=d, ctx);
//     let+ tok = Delim.is_tok(delim);
//     let (stepped, exited) = hstep_tok(d, tok);
//     let sel = Selection.{focus: d, range: Zigg.of_tok(stepped)};
//     // if token still has cursor, then duplicate on ctx
//     let ctx = exited ? ctx : Ctx.push(~onto=d, stepped, ctx);
//     Zipper.mk(~cur=Select(sel), ctx);
//   | Point(Within(tok)) =>
//     let (stepped, exited) = hstep_tok(d, tok);
//     let sel = Selection.{focus: d, range: Zigg.of_tok(stepped)};
//     let ctx =
//       ctx
//       // anchor remains within token so duplicate on b side
//       |> Ctx.push(~onto=b, stepped)
//       // duplicate token on d side if focus remains within token
//       |> (exited ? Fun.id : Ctx.push(~onto=d, stepped));
//     return(Zipper.mk(~cur=Select(sel), ctx));
//   | Select((site_l, site_r)) =>
//     let sel = Option.get(Cursor.get_select(z.cur));
//     let (site_foc, site_anc) = Dir.order(sel.focus, (site_l, site_r));
//     let _ =
//       switch (Zigg.is_tok(sel.range)) {
//       | Some(tok) =>
//         switch (site_foc) {
//         | Between => x
//         | Within(_) =>
//           let (stepped, exited) = hstep_tok(d, tok);
//           if (d == sel.focus) {
//             // grow selection
//             let sel = {...sel, range: Zigg.of_tok(stepped)};
//             let ctx =
//               ctx
//               |> (exited ? Fun.id : Ctx.push(~onto=d, stepped))
//               |> (
//                 Site.is_between(site_anc)
//                   ? Fun.id : Ctx.push(~onto=b, stepped)
//               );
//             return(Zipper.mk(~cur=Select(sel), ctx));
//           } else {
//             // shrink selection
//             let cur =
//               switch (stepped.marks, site_anc) {
//               // selection shrunk to point
//               | (Some(Point({hand: Focus, _})), Within(_))
//               | (None, Between) => Cursor.Point(Caret.focus())
//               | _ => Select({...sel, range: Zigg.of_tok(stepped)})
//               };
//             let ctx =
//               ctx
//               |> Ctx.push(~onto=d, stepped)
//               |> (
//                 switch (stepped.marks) {
//                 | None
//                 | Some(Point(_)) => Fun.id
//                 | Some(Select(_)) => Ctx.push(~onto=b, stepped)
//                 }
//               );
//             return(Zipper.mk(~cur, ctx));
//           };
//         }
//       | None => x
//       };

//     let/ () = {
//       let* tok = Zigg.is_tok(sel.range);
//       // this path covers both growing and shrinking selection
//       let (stepped, exited) = hstep_tok(d, tok);
//       let cur =
//         switch (stepped.marks) {
//         | Some(Point({hand: Focus, _})) => Cursor.Point(Caret.focus())
//         | _ => Select({...sel, range: Zigg.of_tok(stepped)})
//         };
//       let ctx =
//         ctx
//         // anchor remains within token so duplicate on b side
//         |> Ctx.push(~onto=b, stepped)
//         // duplicate token on d side if focus remains within token
//         |> (exited ? Fun.id : Ctx.push(~onto=d, stepped, ~fill=Cell.empty));
//       return(Zipper.mk(~cur, ctx));
//     };

//     let (site_foc, site_anc) = Dir.order(sel.focus, (site_l, site_r));

//     switch (site_foc, site_anc) {
//     // selection is within a single token
//     | (Within(tok_foc), Within(tok_anc))
//         when Token.merges(tok_foc, tok_anc) =>
//       // this path covers both growing and shrinking selection
//       let (stepped, exited) = hstep_tok(d, tok_foc);
//       let cur =
//         switch (stepped.marks) {
//         | Some(Point({hand: Focus, _})) => Cursor.Point(Caret.focus())
//         | _ => Select({...sel, range: Zigg.of_tok(stepped)})
//         };
//       let ctx =
//         ctx
//         // anchor remains within token so duplicate on b side
//         |> Ctx.push(~onto=b, stepped)
//         // duplicate token on d side if focus remains within token
//         |> (exited ? Fun.id : Ctx.push(~onto=d, stepped, ~fill=Cell.empty));
//       return(Zipper.mk(~cur, ctx));
//     | (Within(tok), _) =>
//       let (stepped, exited) = hstep_tok(d, tok);
//       if (sel.focus == d) {
//         let sel = Selection.map(Zigg.map_face(Fun.const(stepped)), sel);
//         let ctx = exited ? ctx : Ctx.push(~onto=d, stepped, ctx);
//         return(Zipper.mk(~cur=Select(sel), ctx));
//       } else {
//         let sel = Selection.map(Z);
//         ();
//       };
//     | Between => x
//     };
//   | _ => x
//   };
// };

// let hstep = (d: Dir.t, z: Zipper.t): option(Zipper.t) => {
//   open Options.Syntax;
//   let b = Dir.toggle(d);
//   switch (z.cur) {
//   | Point(_) =>
//     // P.log("--- Select.hstep/Point ---");
//     let (delim, ctx) = Ctx.pull(~from=d, z.ctx);
//     // P.show("pulled delim", Delim.show(delim));
//     // P.show("pulled ctx", Ctx.show(ctx));
//     let+ tok = Delim.is_tok(delim);
//     // let+ (tok, ctx) = Ctx.pull(~from=d, z.ctx);
//     let (stepped, exited) = hstep_tok(d, tok);
//     let ctx = exited ? ctx : Ctx.push(~onto=d, stepped, ctx);
//     // P.show("hstepped tok", Token.show(tok));
//     // P.show("hstepped ctx", Ctx.show(ctx));
//     let sel = Selection.{focus: d, range: Zigg.of_tok(stepped)};
//     Zipper.mk(~cur=Select(sel), ctx);
//   | Select({focus: side, range: zigg}) =>
//     if (side == d) {
//       let (delim, ctx) = Ctx.pull(~from=d, z.ctx);
//       let+ tok = Delim.is_tok(delim);
//       // let+ (tok, ctx) = Ctx.pull(~from=d, z.ctx);
//       let (tok, ctx) =
//         switch (hstep_tok(d, tok)) {
//         | Error(tok) => (tok, ctx)
//         | Ok(tok) => (tok, Ctx.push(~onto=d, tok, z.ctx))
//         };
//       let zigg = Zigg.grow(~side, tok, zigg);
//       Zipper.mk(~cur=Select({focus: d, range: zigg}), ctx);
//     } else {
//       let (tok, rest) = Zigg.pull(~side, zigg);
//       let (tok, cur) =
//         switch (hstep_tok(d, tok), rest) {
//         | (Error(tok), None) => (tok, Cursor.Point(Caret.focus()))
//         | (Error(tok), Some(zigg)) => (
//             tok,
//             Select(Selection.{focus: side, range: zigg}),
//           )
//         | (Ok(tok), None) => (
//             tok,
//             Select({focus: side, range: Zigg.of_tok(tok)}),
//           )
//         | (Ok(tok), Some(zigg)) => (
//             tok,
//             Select({focus: side, range: Zigg.grow(~side, tok, zigg)}),
//           )
//         };
//       Ctx.push(~onto=b, tok, z.ctx)
//       |> Zipper.mk(~cur)
//       |> Zipper.button
//       |> Option.some;
//     }
//   };
// };

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
