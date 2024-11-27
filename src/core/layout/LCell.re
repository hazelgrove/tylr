open Stds;

include Cell.Base;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Cell.Base.t(Block.t);

let empty = Cell.Base.empty;

let sort = (c: t) =>
  switch (c.meld) {
  | None => Mtrl.Space()
  | Some(m) => LMeld.sort(m)
  };

let map = (f, c: t) => {...c, meld: Option.map(f, c.meld)};

let rec height = (t: t) =>
  t.meld |> Option.map(LMeld.height(~height)) |> Option.value(~default=0);

let rec flatten = (c: t): Block.t =>
  c.meld
  |> Option.map(LMeld.flatten(~flatten))
  |> Option.value(~default=Block.nil);

let flatten_chain = (c: Chain.t(t, Block.t)) =>
  c
  |> Chain.fold_right(
       (t, b, acc) => Block.hcats([flatten(t), b, acc]),
       flatten,
     );
let flatten_affix = (~side: Dir.t, (bs, ts): Chain.Affix.t(Block.t, t)) =>
  List.combine(bs, ts)
  |> List.concat_map(((b, t)) => [b, flatten(t)])
  |> Dir.pick(side, (List.rev, Fun.id))
  |> Block.hcats;

let is_space = (t: t) =>
  switch (t.meld) {
  | None => true
  | Some(M(_, W(w), _)) => List.for_all(Block.is_space, Chain.loops(w))
  };

let rec end_path = (~side: Dir.t, t: t) =>
  switch (t.meld) {
  | None => []
  | Some(M(l, _, r) as m) =>
    switch (side) {
    | L => [0, ...end_path(~side, l)]
    | R => [Chain.length(Meld.Base.to_chain(m)) - 1, ...end_path(~side, r)]
    }
  };

let rec unnest_ft = (n: int, t: t) =>
  n == 0
    ? (t, true)
    : t.meld
      |> Option.map(LMeld.unnest_ft(~unnest_ft, n))
      |> Option.map(Tuples.map_fst(Cell.Base.wrap))
      |> Option.value(~default=(t, false));

let rec nest_tl = (n: int) =>
  map(n == 0 ? Fun.id : LMeld.nest_tl(~nest_tl, n));

let nest_body = (n: int, t: t) => t |> nest_tl(n) |> unnest_ft(n) |> fst;
let nest_body_meld = (n: int, m: LMeld.t) =>
  m |> LMeld.nest_tl(~nest_tl, n) |> LMeld.unnest_ft(~unnest_ft, n) |> fst;

let rec depad = (~side: Dir.t, c: t) =>
  switch (c.meld) {
  | None => (c, empty)
  | Some(M(_, W((bs, _)), _)) when List.for_all(Block.is_space, bs) => (
      c,
      empty,
    )
  | Some(M(l, w, r)) =>
    switch (side) {
    | L =>
      let (p_l, l) = depad(~side, l);
      (p_l, Cell.Base.wrap(M(l, w, r)));
    | R =>
      let (p_r, r) = depad(~side, r);
      (p_r, Cell.Base.wrap(M(l, w, r)));
    }
  };
