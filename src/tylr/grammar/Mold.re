[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t = {
  sort: Mtrl.Sorted.t,
  prec: Prec.t,
  rctx: RCtx.t(Mtrl.Sym.t),
};

let equal = (==);

let nullable = (~side: Dir.t, m: t) => RCtx.nullable(side, m.rctx);

let push = (~onto: Dir.t, msym: Mtrl.Sym.t, mold: t) => {
  ...mold,
  rctx: RCtx.push(~onto, Atom(msym), mold.rctx),
};
