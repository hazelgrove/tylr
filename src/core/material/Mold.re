[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t = {
  rctx: RCtx.t(Grammar.Sym.t),
  prec: Prec.t,
  sort: Sort.t,
};

let mk = (~rctx=RCtx.empty, ~prec, sort) => {rctx, prec, sort};

let is_null = (~side: Dir.t, m: t) =>
  RCtx.is_null(~atom=Fun.const(false), ~side, m.rctx);
let nullable = (~side: Dir.t, m: t) =>
  RCtx.nullable(~atom=Fun.const(false), ~side, m.rctx);
let t_nullable = (~side: Dir.t, m: t) =>
  RCtx.nullable(~atom=Sym.is_nt, ~side, m.rctx);

// only sensical for NT molds
let bounds = m =>
  Bound.(
    nullable(~side=R, m) ? Node(m.prec) : Root,
    nullable(~side=L, m) ? Node(m.prec) : Root,
  );

let push = (~onto: Dir.t, sym: Grammar.Sym.t, mold: t) => {
  ...mold,
  rctx: RCtx.push(~onto, Atom(sym), mold.rctx),
};

let display = (m: t) => (
  is_null(~side=L, m) ? "<" : nullable(~side=L, m) ? "|" : ">",
  is_null(~side=R, m) ? ">" : nullable(~side=R, m) ? "|" : "<",
);
