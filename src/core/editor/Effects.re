type t =
  | Insert(Token.t)
  | Remove(Token.t);

let log = ref([]);

let perform = (eff: t) => log := [eff, ...log^];
let insert = tok => perform(Insert(tok));
let remove = tok => perform(Remove(tok));
// let merge = (l, r) => perform(Merge(l, r));

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

let commit = (effs: list(t)) => List.iter(perform, effs);
