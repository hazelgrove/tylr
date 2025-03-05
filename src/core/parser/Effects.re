[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Insert(Token.t)
  | Remove(Token.t);

let log = ref([]);
let reset = () => log := [];

let print = () => {
  open Stds;
  P.log("effects:");
  log^ |> List.iter(eff => P.sexp("eff", sexp_of_t(eff)));
};
let oprint = x => {
  print();
  x;
};

let invert = (eff: t): t =>
  switch (eff) {
  | Insert(tok) => Remove(tok)
  | Remove(tok) => Insert(tok)
  };

let perform = (eff: t) => {
  log :=
    List.mem(invert(eff), log^)
      ? List.filter(eff' => eff' != invert(eff), log^) : [eff, ...log^];
};
let insert = tok => {
  perform(Insert(tok));
  tok;
};
let remove = tok => perform(Remove(tok));
// let merge = (l, r) => perform(Merge(l, r));

let is_fresh_tile = (tok: Token.t) =>
  Mtrl.is_tile(tok.mtrl)
  && !Token.is_empty(tok)
  && List.mem(Insert(tok), log^);

let perform_if = (eff: t, opt: option(_)) => {
  if (Option.is_some(opt)) {
    perform(eff);
  };
  opt;
};

// apply f to x, record effects, and emit as values without commiting log.
// useful for choosing between numerous effectful paths and choosing one based
// on their results, after which caller should call commit on chosen effects.
let dry_run = (f, x): (_, list(t)) => {
  let saved = log^;
  log := [];
  let y = f(x);
  let recorded = log^;
  log := saved;
  (y, recorded);
};

let dry_run_lazy = (x: Lazy.t(_)): (_, list(t)) => {
  let saved = log^;
  log := [];
  let y = Lazy.force(x);
  let recorded = log^;
  log := saved;
  (y, recorded);
};

let commit = (effs: list(t)) => List.iter(perform, effs);
