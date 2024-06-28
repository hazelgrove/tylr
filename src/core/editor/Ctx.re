open Stds;

include Chain;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Chain.t(Frame.Open.t, Frame.Closed.t);

let empty = Chain.unit(Frame.Open.empty);

let link = (~open_=Frame.Open.empty) => link(open_);

let fold = Chain.fold_left;
let fold_root = Chain.fold_right;

let face = (~side: Dir.t, ctx: t) => {
  open Stds.Options.Syntax;
  let/ () = Frame.Open.face(~side, hd(ctx));
  let+ (_, (l, r), _) = Result.to_option(unlink(ctx));
  Terr.face(Dir.pick(side, (l, r)));
};

// let bound = (~side: Dir.t, ctx) =>
//   switch (unlink(ctx)) {
//   | Error(_) => Bound.Root
//   | Ok((_, closed, _)) => Node()
//   };

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
let add = ((pre, suf): (Meld.Affix.t, Meld.Affix.t)) => {
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
      Some((tok, link(~open_=order((s_d, s_b)), terrs, ctx)))
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

let extend = (~side: Dir.t, tl: Chain.Affix.t(Cell.t, Token.t)) =>
  map_hd(Frame.Open.extend(~side, tl));

let push_opt =
    (~onto as d: Dir.t, t: Token.t, ~fill=Cell.empty, ctx: t): option(t) => {
  open Options.Syntax;
  let (hd, tl) = uncons(ctx);
  let (s_d, s_b) = Dir.order(d, hd);
  let (b_d, b_b) = Dir.order(d, Tl.bounds(tl));
  let+ melded = Melder.push(~onto=d, t, ~fill, s_d, ~bound=b_d);
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
let push = (~onto: Dir.t, tok, ~fill=Cell.empty, ctx) =>
  push_opt(~onto, tok, ~fill, ctx)
  |> Options.get_exn(Invalid_argument("Ctx.push"));

let push_wald_opt = (~onto: Dir.t, w: Wald.t, ~fill=Cell.empty, ctx) => {
  let (hd, tl) = Wald.uncons(w);
  push_opt(~onto, hd, ~fill, ctx) |> Option.map(extend(~side=onto, tl));
};
let push_wald = (~onto: Dir.t, w: Wald.t, ~fill=Cell.empty, ctx) =>
  push_wald_opt(~onto, w, ~fill, ctx)
  |> Options.get_exn(Invalid_argument("Ctx.push_wald"));

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

let mold = (ctx: t, tok: Token.Unmolded.t): t => {
  let ((dn, up), tl) = uncons(ctx);
  let (l, r) = Tl.bounds(tl);
  switch (Molder.mold(~bound=l, dn, tok)) {
  | Neq(dn) => cons((dn, up), tl)
  | Eq(l) =>
    let (dn, up) = ([l], Slope.cat(up, Bound.to_list(r)));
    map_hd(Frame.Open.cat((dn, up)), Tl.rest(tl));
  };
};

let rec remold = (~fill=Cell.empty, ctx: t): (Cell.t, t) => {
  let ((dn, up), tl) = uncons(ctx);
  switch (Slope.unlink(up)) {
  | Some((tok, cell, up)) when Token.Grout.is(tok) =>
    Effects.remove(tok);
    let up = Slope.cat(Slope.Up.unroll(cell), up);
    remold(~fill, cons((dn, up), tl));
  | _ =>
    let (l, r) = Tl.bounds(tl);
    switch (up) {
    | [] =>
      let ctx = cons(Frame.Open.empty, tl);
      (Melder.complete_bounded(~bounds=(l, r), ~onto=L, dn, ~fill), ctx);
    | [hd, ...up_tl] =>
      let (molded, rest) = Molder.remold(~bound=l, dn, ~fill, hd);
      let (fill, up) =
        switch (rest) {
        | Ok(cell) => (cell, up_tl)
        | Error(up) => (Cell.empty, Slope.cat(up, up_tl))
        };
      let ctx =
        switch (molded) {
        | Neq(dn) => cons((dn, up), tl)
        | Eq(l) =>
          let (dn, up) = ([l], Slope.cat(up, Bound.to_list(r)));
          map_hd(Frame.Open.cat((dn, up)), Tl.rest(tl));
        };
      remold(~fill, ctx);
    };
  };
};

let zip_toks = (~caret=?, ctx: t): option((Meld.t, t)) => {
  let (hd, tl) = uncons(ctx);
  Frame.Open.zip_toks(~caret?, hd)
  |> Option.map(Tuples.map_snd(hd => cons(hd, tl)));
};
