include Meld.Base;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Meld.Base.t(Block.t);

let sort = (M(_, w, _): t) => LWald.sort(w);

let flatten = (~flatten, M(l, w, r): t): Block.t =>
  Block.hcats([flatten(l), LWald.flatten(~flatten, w), flatten(r)]);

let height = (~height, M(l, W((toks, _)), r): t) =>
  toks
  |> List.map(Block.height)
  |> List.fold_left((+), height(l) + (+ height(r)));

let unnest_ft = (~unnest_ft, n, M(l, w, r): t): (t, bool) => {
  let (r, done_) = unnest_ft(n, r);
  let (w, done_) = done_ ? (w, true) : LWald.unnest_ft(~unnest_ft, n, w);
  let (l, done_) = done_ ? (l, true) : unnest_ft(n, l);
  (M(l, w, r), done_);
};

let nest_tl = (~nest_tl, n, M(l, W(w), r): t): t =>
  M(
    nest_tl(n, l),
    W(Chain.map_loop(Block.nest_tl(n), w)),
    nest_tl(n, r),
  );
