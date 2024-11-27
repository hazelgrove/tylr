include Ctx.Base;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Ctx.Base.t(Block.t);

// todo: rename
let add = ((pre, suf)) =>
  switch (LTerr.mk'(pre), LTerr.mk'(suf)) {
  | (None, None) => Fun.id
  | (None, Some(r)) => Chain.map_hd(LFrame.Open.cons(~onto=R, r))
  | (Some(l), None) => Chain.map_hd(LFrame.Open.cons(~onto=L, l))
  | (Some(l), Some(r)) =>
    Mtrl.is_space(LTerr.sort(l)) && Mtrl.is_space(LTerr.sort(r))
      ? Chain.map_hd(((dn, up)) => ([l, ...dn], [r, ...up]))
      : Chain.link(LFrame.Open.empty, (l, r))
  };

let snoc = (~side: Dir.t, terr: LTerr.t, ctx: t) =>
  ctx |> Chain.map_ft(LFrame.Open.snoc(~side, terr));

let flatten = (ctx: t): (LEqs.t, Frame.Open.Base.t('tok)) =>
  ctx
  |> Chain.fold_right(
       ((dn, up), (l, r), ((len_l, len_r), (eqs, flat))) => {
         let flat =
           flat |> Frame.Open.cat(([l], [r])) |> Frame.Open.cat((dn, up));
         let eqs = [(len_l, len_r), ...eqs];
         let lens = (
           List.length(dn) + 1 + len_l,
           List.length(up) + 1 + len_r,
         );
         (lens, (eqs, flat));
       },
       ((dn, up)) => (List.(length(dn), length(up)), ([], (dn, up))),
     )
  |> snd;
