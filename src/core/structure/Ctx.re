open Util;

include Chain;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Chain.t(Frame.Open.t, Frame.Closed.t);

let empty = Chain.unit(Frame.Open.empty);

let link = (~slopes=Frame.Open.empty) => link(slopes);

let fold = Chain.fold_left;
let fold_root = Chain.fold_right;

let face = (~side: Dir.t, ctx: t) => {
  open Util.Options.Syntax;
  let/ () = Frame.Open.face(~side, hd(ctx));
  let+ (_, (l, r), _) = Result.to_option(unlink(ctx));
  Terr.face(Dir.pick(side, (l, r)));
};

let extend = (~side as d: Dir.t, tl, ctx) => {
  switch (Frame.Open.extend(~side=d, tl, hd(ctx))) {
  | Some(hd) => Some(put_hd(hd, ctx))
  | None =>
    open Util.Options.Syntax;
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
