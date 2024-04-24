type t =
  | Insert(Token.t)
  | Remove(Token.t);

let log = ref([]);

let perform = eff => log := [eff, ...log^];
let insert = tok => perform(Insert(tok));
let remove = tok => perform(Remove(tok));

// necessary to wrap recorded effects in existential wrapper for reasons
// I don't fully understand:
// https://stackoverflow.com/questions/49538251/heterogeneous-list-of-gadt-in-ocaml
// type recorded =
//   | R(t('a)): recorded;

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

// let perform_all = set =>
//   to_list(set)
//   |> List.iter(((_, eff)) => Effect.perform(eff));

// let perform_if = (o, eff: t(unit)) =>
//   switch (o) {
//   | None => None
//   | Some(x) =>
//     perform(eff);
//     Some(x);
//   };
