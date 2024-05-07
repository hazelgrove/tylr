open Util;

[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t = {
  rctx: RCtx.t(Mtrl.Sym.t),
  prec: Prec.t,
  sort: Mtrl.Sorted.t,
};

let equal = (==);

let is_null = (~side: Dir.t, m: t) =>
  RCtx.is_null(~atom=Mtrl.Sym.is_null, ~side, m.rctx);
let nullable = (~side: Dir.t, m: t) =>
  RCtx.nullable(~atom=Mtrl.Sym.is_null, ~side, m.rctx);

let bounds = m =>
  Bound.(
    nullable(~side=L, m) ? Node(m.prec) : Root,
    nullable(~side=R, m) ? Node(m.prec) : Root,
  );

let push = (~onto: Dir.t, msym: Mtrl.Sym.t, mold: t) => {
  ...mold,
  rctx: RCtx.push(~onto, Atom(msym), mold.rctx),
};

let display = (m: t) => (
  is_null(~side=L, m) ? "<" : nullable(~side=L, m) ? "|" : ">",
  is_null(~side=R, m) ? ">" : nullable(~side=R, m) ? "|" : "<",
);
