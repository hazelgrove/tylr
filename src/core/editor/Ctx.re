open Stds;

include Chain;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Chain.t(Frame.Open.t, Frame.Closed.t);

let empty = Chain.unit(Frame.Open.empty);

let link = (~slopes=Frame.Open.empty) => link(slopes);

let fold = Chain.fold_left;
let fold_root = Chain.fold_right;

let face = (~side: Dir.t, ctx: t) => {
  open Stds.Options.Syntax;
  let/ () = Frame.Open.face(~side, hd(ctx));
  let+ (_, (l, r), _) = Result.to_option(unlink(ctx));
  Terr.face(Dir.pick(side, (l, r)));
};

let bound = (~side: Dir.t, ctx) =>
  switch (unlink(ctx)) {
  | Error(_) => Bound.Root
  | Ok((_, closed, _)) => Node()
  };

let flatten =
  Chain.fold_right(
    (open_, (l, r), acc) =>
      acc
      |> Frame.Open.cons(~onto=L, l)
      |> Frame.Open.cons(~onto=R, r)
      |> Frame.Open.cat(open_),
    Fun.id,
  );

// todo: rename
let cons = ((pre, suf): (Meld.Affix.t, Meld.Affix.t)) => {
  let l = () => Terr.mk(fst(pre), snd(pre));
  let r = () => Terr.mk(fst(suf), snd(suf));
  if (Meld.Affix.is_empty(pre)) {
    map_hd(((dn, up)) => (dn, [r(), ...up]));
  } else if (Meld.Affix.is_empty(suf)) {
    map_hd(((dn, up)) => ([l(), ...dn], up));
  } else {
    link((l(), r()));
  };
};

let rec pull = (~from as d: Dir.t, ctx: t): option((Token.t, t)) => {
  open Options.Syntax;
  let order = Dir.order(d);
  switch (unlink(ctx)) {
  | Error(slopes) =>
    let (s_d, s_b) = order(slopes);
    let+ (tok, s_d) = Slope.pull(~from=d, s_d);
    (tok, unit(order((s_d, s_b))));
  | Ok((slopes, terrs, ctx)) =>
    let (s_d, s_b) = order(slopes);
    switch (Slope.pull(~from=d, s_d)) {
    | Some((tok, s_d)) =>
      Some((tok, link(~slopes=order((s_d, s_b)), terrs, ctx)))
    | None =>
      let (t_d, t_b) = Dir.order(d, terrs);
      let slopes = order(([t_d], s_b @ [t_b]));
      pull(~from=d, map_hd(Frame.Open.cat(slopes), ctx));
    };
  };
};

module Tl = {
  include Chain.Affix;
  type t = Chain.Affix.t(Frame.Closed.t, Frame.Open.t);
  let bounds =
    fun
    | ([], _)
    | (_, []) => Bound.(Root, Root)
    | ([(l, r), ..._], _) => (Node(l), Node(r));
  let rest =
    fun
    | ([], _)
    | (_, []) => empty
    | ([_, ...cs], os) => Chain.mk(os, cs);
  // let tl = b => b |> Bound.map(snd) |> Bound.get(~root=empty);
  // let hd = b => b |> Bound.map(fst) |> Bound.split;
};

// let split_bound = (ctx: t): (Frame.Open.t, Bound.t) =>
//   switch (unlink(ctx)) {
//   | Error(open_) => (open_, Bound.Root)
//   | Ok((open_, closed, tl)) => (open_, Node((closed, tl)))
//   };
// let merge_bound = (open_: Frame.Open.t, bound: Bound.t) =>
//   switch (bound) {
//   | Root => unit(open_)
//   | Node((closed, ctx)) => link(open_, closed, ctx)
//   };

let push_wald_opt =
    (~onto as d: Dir.t, w: Wald.t, ~fill=Cell.empty, ctx: t): option(t) => {
  open Options.Syntax;
  let (hd, tl) = split_hd(ctx);
  let (s_d, s_b) = Dir.order(d, hd);
  let (b_d, b_b) = Dir.order(d, Tl.bounds(tl));
  let+ melded = Melder.push_bounded(~onto=d, w, ~fill, s_d, ~bound=b_d);
  switch (melded) {
  | Neq(s_d) =>
    let (dn, up) = Dir.order(d, (s_d, s_b));
    cons((dn, up), tl);
  | Eq(b_d) =>
    let s_b = Slope.cat(s_b, Bound.to_list(b_b));
    let open_ = Dir.order(d, ([b_d], s_b));
    map_hd(Frame.Open.cat(open_), Tl.rest(tl));
  };
};
let push_wald = (~onto: Dir.t, wald, ~fill=Cell.empty, ctx) =>
  push_wald_opt(~onto, wald, ~fill, ctx)
  |> Options.get_exn(Invalid_argument("Ctx.push_wald"));

let push_tok_opt = (~onto: Dir.t, tok: Token.t) =>
  push_wald_opt(~onto, Wald.of_tok(tok));
let push_tok = (~onto: Dir.t, tok: Token.t) =>
  push_wald(~onto, Wald.of_tok(tok));

let rec push_slope = (~onto: Dir.t, s: Slope.t, ~fill=Cell.empty, ctx: t) =>
  switch (s) {
  | [] => ctx
  | [hd, ...tl] =>
    let ctx = push_wald(~onto, hd.wald, ~fill, ctx);
    push_slope(~onto, tl, ~fill=hd.cell, ctx);
  };
let push_zigg = (~onto as d: Dir.t, zigg: Zigg.t, ~fill=Cell.empty, ctx: t) => {
  let (s_d, top, s_b) = Zigg.orient(d, zigg);
  let ctx = push_slope(~onto=d, s_d, ~fill, ctx);
  // need to propagate given fill if push_slope did not incorporate
  let fill = Stds.Lists.is_empty(s_d) ? fill : Cell.empty;
  let ctx = push_wald(~onto=d, top, ~fill, ctx);
  let rest = Dir.order(d, ([], s_b));
  map_hd(Frame.Open.cat(rest), ctx);
};

let close = (~sel=?, ctx: t) => {
  let rec go = (ctx: t) =>
    switch (hd(ctx)) {
    | ([], _)
    | (_, []) => ctx
    | ([l, ..._] as pre, [r, ...suf]) when Melder.lt(l.wald, r.wald) =>
      ctx |> put_hd((pre, suf)) |> go |> map_hd(Frame.Open.cons(~onto=R, r))
    | ([l, ...pre], [r, ..._] as suf) when Melder.gt(l.wald, r.wald) =>
      ctx |> put_hd((pre, suf)) |> go |> map_hd(Frame.Open.cons(~onto=L, l))
    | ([l, ...pre], [r, ...suf]) =>
      assert(Melder.eq(l.wald, r.wald));
      ctx |> put_hd((pre, suf)) |> go |> link((l, r));
    };
  switch (sel) {
  | None => go(ctx)
  | Some(zigg) =>
    let (pre, suf) = hd(ctx);
    let (pre_lt, pre_geq) = Zigg.take_geq(pre, zigg);
    let (suf_leq, suf_gt) = Zigg.take_leq(zigg, suf);
    ctx
    |> put_hd((pre_lt, suf_gt))
    |> go
    |> map_hd(Frame.Open.cat((pre_geq, suf_leq)));
  };
};

let remold_terr = (~bound, dn, ~fill, terr): (Melded.t, Cell.t, Slope.Up.t) => {
  let (hd, tl) = Wald.uncons(terr.wald);
  let molded = Molder.mold(~bound, dn, ~fill, Token.unmold(hd));
  if (Melded.face(molded) == hd.mtrl) {
    (
      // fast path for when terr hd retains original mtrl
      Melded.extend(tl, molded),
      Chain.unit(terr.cell),
      Slope.empty,
    );
  } else {
    let up =
      Chain.Affix.uncons(tl)
      |> Option.map((cell, (ts, cs)) =>
           Slope.Up.unroll(cell) @ [{...terr, wald: Wald.mk(ts, cs)}]
         )
      |> Option.value(~default=Slope.empty);
    (molded, Cell.empty, up);
  };
};

let remold = (~fill=Cell.empty, ctx: t): (Cell.t, Tl.t) => {
  let ((dn, up), tl) = split_hd(ctx);
  switch (Slope.unlink(up)) {
  | Some((tok, cell, up)) when Token.Grout.is(tok) =>
    Effects.remove(tok);
    let up = Slope.cat(Slope.Up.unroll(cell), up);
    remold(~fill, cons((dn, up), tl));
  | _ =>
    switch (up) {
    | [] => (Melder.complete_slope(~onto=L, dn, ~fill), tl)
    | [hd, ...tl] =>
      let (l, r) = Tl.bounds(tl);
      let (molded, rest) = Molder.remold(~bound=l, dn, ~fill, hd);
      let (fill, up) =
        switch (rest) {
        | Ok(cell) => (cell, tl)
        | Error(up) => (Cell.empty, Slope.cat(up, tl))
        };
      let ctx =
        switch (molded) {
        | Neq(dn) => cons((dn, up), tl)
        | Eq(l) =>
          let (dn, up) = ([l], Slope.cat(up, Bound.to_list(r)));
          map_hd(Frame.Open.cat((dn, up)), Tl.rest(tl));
        };
      remold(~fill, ctx);
    }
  };
};

let rec remold = (~fill=Cell.empty, ctx: t): (Cell.t, Ctx.t) => {
  let ((dn, up), bound) = Ctx.split_bound(ctx);
  switch (Slope.unlink(up)) {
  | Some((tok, cell, up)) when Token.Grout.is(tok) =>
    Effects.remove(tok);
    let up = Slope.cat(Slope.Up.unroll(cell), up);
    remold(~fill, merge_bound((dn, up), bound));
  | _ =>
    switch (up) {
    | [] =>
      let cell = Melder.complete_slope(~onto=L, dn, ~fill);
      let ctx = merge_bound(Frame.Open.empty, bound);
      (cell, ctx);
    | [hd, ...tl] =>
      let (closed, ctx) = Bound.split(bound);
      let (l, r) = Bound.split(closed);
      let (tok, rest) = Wald.split_hd(hd.wald);
      let molded =
        Molder.mold(~bound=l, dn, ~fill, Token.Unmolded.unmold(tok));
      let (molded, fill, up) =
        if (Melded.face(molded) == tok.mtrl) {
          // fast path for when tok retains original mold
          let extended = Melded.extend(rest, molded);
          (extended, hd.cell, tl);
        } else {
          let (cell, up) =
            switch (Chain.Affix.split_hd(rest)) {
            | None => (hd.cell, tl)
            | Some((cell, (ts, cs))) =>
              let hd = {...hd, wald: Wald.mk(ts, cs)};
              (cell, [hd, ...tl]);
            };
          (molded, Cell.empty, Slope.(cat(Up.unroll(cell), up)));
        };
      let ctx =
        switch (molded) {
        | Neq(dn) => merge_bound((dn, up), bound)
        | Eq(l) =>
          ctx
          |> Bound.get(~root=empty)
          |> map_hd(
               Frame.Open.cat(([l], Slope.cat(up, Bound.to_list(r)))),
             )
        };
      remold(~fill, ctx);
    }
  };
};
