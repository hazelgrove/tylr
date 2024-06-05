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

let extend = (~side as d: Dir.t, tl, ctx) => {
  switch (Frame.Open.extend(~side=d, tl, hd(ctx))) {
  | Some(hd) => Some(put_hd(hd, ctx))
  | None =>
    open Stds.Options.Syntax;
    let+ (slopes, terrs, ctx) = Result.to_option(unlink(ctx));
    let (t_d, t_b) = Dir.order(d, terrs);
    let terrs = Dir.order(d, (Terr.extend(tl, t_d), t_b));
    link(~slopes, terrs, ctx);
  };
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

let push_wald =
    (~onto as d: Dir.t, w: Wald.t, ~fill=Fill.empty, ctx: t): option(t) => {
  open Options.Syntax;
  let (slopes, tl) = split_hd(ctx);
  let (s_d, s_b) = Dir.order(d, slopes);
  switch (Melder.Slope.push(~onto=d, w, ~fill, s_d)) {
  | Ok(s_d) =>
    let slopes = Dir.order(d, (s_d, s_b));
    Some(zip(slopes, ~suf=tl));
  | Error(fill) =>
    switch (Affix.split_hd(tl)) {
    | None =>
      let+ s_d = Melder.Wald.meld_root(~from=d, ~fill, w);
      unit(Dir.order(d, (s_d, s_b)));
    | Some((terrs, ctx)) =>
      let (t_d, t_b) = Dir.order(d, terrs);
      let+ melded = Melder.Wald.meld(~from=d, t_d.wald, ~fill, w);
      switch (melded) {
      | (s_d, wald) when wald == t_d.wald =>
        let slopes = Dir.order(d, (s_d, s_b));
        link(~slopes, terrs, ctx);
      | (s_d, wald) =>
        let slopes =
          Dir.order(d, (s_d @ [Terr.{...t_d, wald}], s_b @ [t_b]));
        map_hd(Frame.Open.cat(slopes), ctx);
      };
    }
  };
};
let push = (~onto: Dir.t, t: Token.t) => push_wald(~onto, Wald.of_tok(t));
let push_fail = (~onto: Dir.t, tok, ctx) =>
  push(~onto, tok, ctx) |> Options.get_fail("bug: failed push");

let rec push_slope = (~onto: Dir.t, s: Slope.t, ~fill=Fill.empty, ctx: t) =>
  switch (s) {
  | [] => Some(ctx)
  | [hd, ...tl] =>
    open Options.Syntax;
    let* ctx = push_wald(~onto, hd.wald, ~fill, ctx);
    push_slope(~onto, tl, ~fill=Fill.unit(hd.cell), ctx);
  };
let push_zigg = (~onto as d: Dir.t, zigg: Zigg.t, ~fill=Fill.empty, ctx: t) => {
  let get_fail = Options.get_fail("bug: failed to push zigg");
  let (s_d, top, s_b) = Zigg.orient(d, zigg);
  let ctx = get_fail(push_slope(~onto=d, s_d, ~fill, ctx));
  let fill = Stds.Lists.is_empty(s_d) ? fill : Fill.empty;
  let ctx = get_fail(push_wald(~onto=d, top, ~fill, ctx));
  let rest = Dir.order(d, ([], s_b));
  map_hd(Frame.Open.cat(rest), ctx);
};

let rec pull = (~from as d: Dir.t, ctx: t): option((Token.t, t)) => {
  open Options.Syntax;
  let order = Dir.order(d);
  switch (unlink(ctx)) {
  | Error(slopes) =>
    let (s_d, s_b) = order(slopes);
    let+ (tok, s_d) = Melder.Slope.pull(~from=d, s_d);
    (tok, unit(order((s_d, s_b))));
  | Ok((slopes, terrs, ctx)) =>
    let (s_d, s_b) = order(slopes);
    switch (Melder.Slope.pull(~from=d, s_d)) {
    | Some((tok, s_d)) =>
      Some((tok, link(~slopes=order((s_d, s_b)), terrs, ctx)))
    | None =>
      let (t_d, t_b) = Dir.order(d, terrs);
      let slopes = order(([t_d], s_b @ [t_b]));
      pull(~from=d, map_hd(Frame.Open.cat(slopes), ctx));
    };
  };
};

let close = (~sel=?, ctx: t) => {
  let rec go = (ctx: t) =>
    switch (hd(ctx)) {
    | ([], _)
    | (_, []) => ctx
    | ([l, ..._] as pre, [r, ...suf]) when Melder.Wald.lt(l.wald, r.wald) =>
      ctx |> put_hd((pre, suf)) |> go |> map_hd(Frame.Open.cons(~onto=R, r))
    | ([l, ...pre], [r, ..._] as suf) when Melder.Wald.gt(l.wald, r.wald) =>
      ctx |> put_hd((pre, suf)) |> go |> map_hd(Frame.Open.cons(~onto=L, l))
    | ([l, ...pre], [r, ...suf]) =>
      assert(Melder.Wald.eq(l.wald, r.wald));
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
