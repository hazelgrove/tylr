open Stds;

include Chain;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Chain.t(Frame.Open.t, Frame.Closed.t);

let empty = Chain.unit(Frame.Open.empty);

let link = (~open_=Frame.Open.empty) => link(open_);

let fold = Chain.fold_left;
let fold_root = Chain.fold_right;

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
let add = ((pre, suf): (Meld.Affix.t, Meld.Affix.t)) =>
  switch (Terr.mk'(pre), Terr.mk'(suf)) {
  | (None, None) => Fun.id
  | (None, Some(r)) => map_hd(Frame.Open.cons(~onto=R, r))
  | (Some(l), None) => map_hd(Frame.Open.cons(~onto=L, l))
  | (Some(l), Some(r)) =>
    Token.merges(Terr.hd(l), Terr.hd(r))
      ? map_hd(((dn, up)) => ([l, ...dn], [r, ...up])) : link((l, r))
  };

let pull = (~from: Dir.t, ctx: t): (Delim.t, t) => {
  let (hd, tl) = uncons(ctx);
  let (face, hd') = Frame.Open.pull(~from, hd);
  switch (face) {
  | Node(_) => (face, cons(hd', tl))
  | Root =>
    switch (tl) {
    | ([], _empty) => (face, cons(hd', tl))
    | ([c, ...cs], os) =>
      let (t, rest) = Frame.Closed.pull(~from, c);
      let ctx =
        mk(os, cs)
        |> map_hd(Frame.Open.cat(rest))
        |> map_hd(Frame.Open.cat(hd'));
      (Node(t), ctx);
    }
  };
};
let face = (~side: Dir.t, ctx) => fst(pull(~from=side, ctx));

let peel = (~from: Dir.t, tok: Token.t, ctx: t) =>
  switch (pull(~from, ctx)) {
  | (Node(t), rest) when t.id == tok.id => rest
  | _ => ctx
  };

module Tl = {
  include Chain.Affix;
  [@deriving (show({with_path: false}), sexp, yojson)]
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

let push = (~onto: Dir.t, tok: Token.t, ctx) =>
  switch (push_opt(~onto, tok, ctx)) {
  | Some(ctx) => ctx
  | None => raise(Invalid_argument("Ctx.push"))
  };

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

let zip_toks = (~save_cursor=false, ctx: t): option((Meld.t, t)) => {
  let (hd, tl) = uncons(ctx);
  Frame.Open.zip_toks(~save_cursor, hd)
  |> Option.map(Tuples.map_snd(hd => cons(hd, tl)));
};

let zip = (~save_cursor, ~zipped: Cell.t) =>
  fold(Frame.Open.zip(~zipped, ~save_cursor), (zipped, closed, open_) =>
    Frame.Open.zip(open_, ~zipped=Frame.Closed.zip(closed, ~zipped))
  );
let zip_step =
    (~zipped: Cell.t, ctx: t): option((Rel.t(unit, Dir.t), Cell.t, t)) =>
  switch (unlink(ctx)) {
  | Error(open_) =>
    // todo: review these save_cursor flags
    Frame.Open.zip_step(~save_cursor=true, ~zipped, open_)
    |> Option.map(((rel, zipped, open_)) => (rel, zipped, unit(open_)))
  | Ok((open_, (l, r), ctx)) =>
    switch (Frame.Open.zip_step(~save_cursor=true, ~zipped, open_)) {
    | None => Some((Eq(), Frame.zip_eq(l, zipped, r), ctx))
    | Some((rel, zipped, open_)) =>
      Some((rel, zipped, link(~open_, (l, r), ctx)))
    }
  };

// processes open hd of ctx to explicate any latent closed frames. unlike zip,
// button goes top-down and does not require a syntactically complete cell to start.
let button = (ctx: t): t => {
  let ((dn, up), tl) = uncons(ctx);
  let rec go = (~ctx, (rev_dn, rev_up)) =>
    switch (rev_dn, rev_up) {
    | ([], []) => ctx
    | ([], [hd, ...tl]) =>
      let ctx = map_hd(Frame.Open.cons(~onto=R, hd), ctx);
      go(~ctx, (rev_dn, tl));
    | ([hd, ...tl], []) =>
      let ctx = map_hd(Frame.Open.cons(~onto=L, hd), ctx);
      go(~ctx, (tl, rev_up));
    | ([l, ...tl], [r, ..._]) when Melder.lt(l.wald, r.wald) =>
      let ctx = map_hd(Frame.Open.cons(~onto=L, l), ctx);
      go(~ctx, (tl, rev_up));
    | ([l, ..._], [r, ...tl]) when Melder.gt(l.wald, r.wald) =>
      let ctx = map_hd(Frame.Open.cons(~onto=R, r), ctx);
      go(~ctx, (rev_dn, tl));
    | ([l, ...tl_l], [r, ...tl_r])
        when
          Token.merges(Terr.face(l), Terr.face(r))
          || Mtrl.(
               is_space(Terr.face(l).mtrl) && is_space(Terr.face(r).mtrl)
             )
          || !Melder.eq(l.wald, r.wald) =>
      let ctx =
        ctx
        |> map_hd(Frame.Open.cons(~onto=L, l))
        |> map_hd(Frame.Open.cons(~onto=R, r));
      go(~ctx, (tl_l, tl_r));
    | ([l, ...tl_l], [r, ...tl_r]) =>
      assert(Melder.eq(l.wald, r.wald));
      let ctx = link((l, r), ctx);
      go(~ctx, (tl_l, tl_r));
    };
  go(~ctx=cons(Frame.Open.empty, tl), (List.rev(dn), List.rev(up)));
};
