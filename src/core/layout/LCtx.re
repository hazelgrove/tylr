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
