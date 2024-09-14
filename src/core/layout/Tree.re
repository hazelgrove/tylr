// open Sexplib.Std;
// open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Stds;

// todo: unify with existing structure by polymorphizing
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Cell.Base.t(Block.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type meld = Meld.Base.t(Block.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type wald = Wald.Base.t(Block.t);

let empty = Cell.Base.empty;

let map = (f, t: t) => {...t, meld: Option.map(f, t.meld)};

let rec flatten = (t: t): Block.t =>
  t.meld |> Option.map(flatten_meld) |> Option.value(~default=Block.nil)
and flatten_meld = (M(l, w, r): meld): Block.t =>
  Block.hcats([flatten(l), flatten_wald(w), flatten(r)])
and flatten_wald = (W(w): wald) =>
  // hcatting each block-wrapped cells with interleaved single-line tokens
  // will lead to a single block-wrapped element for whole wald
  w |> Chain.to_list(Fun.id, t => Block.wrap(flatten(t))) |> Block.hcats;
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
  | Some(M(_, _, r) as m) => [
      Chain.length(Meld.Base.to_chain(m)) - 1,
      ...end_path(~side, r),
    ]
  };

let rec nest_tl = (n: int) => map(n == 0 ? Fun.id : nest_tl_meld(n))
and nest_tl_meld = (n, M(l, W(w), r): meld) =>
  M(
    nest_tl(n, l),
    W(Chain.map_loop(Block.nest_tl(n), w)),
    nest_tl(n, r),
  );

let rec unnest_ft = (n: int, t: t) =>
  n == 0
    ? (t, true)
    : t.meld
      |> Option.map(unnest_ft_meld(n))
      |> Option.map(Tuples.map_fst(Cell.Base.wrap))
      |> Option.value(~default=(t, false))
and unnest_ft_meld = (n, M(l, w, r): meld): (meld, bool) => {
  let (r, done_) = unnest_ft(n, r);
  let (w, done_) = done_ ? (w, true) : unnest_ft_wald(n, w);
  let (l, done_) = done_ ? (l, true) : unnest_ft(n, l);
  (M(l, w, r), done_);
}
and unnest_ft_wald = (n, W(w): wald) =>
  w
  |> Chain.fold_right_map(
       (b_tok, t, done_) => {
         let (t, done_) = done_ ? (t, true) : unnest_ft(n, t);
         let (b_tok, done_) =
           done_ ? (b_tok, true) : Block.unnest_ft(n, b_tok);
         (b_tok, t, done_);
       },
       Block.unnest_ft(n),
     )
  |> Tuples.map_fst(Wald.Base.w);

let nest_body = (n: int, t: t) => t |> nest_tl(n) |> unnest_ft(n) |> fst;
let nest_body_meld = (n: int, m: meld) =>
  m |> nest_tl_meld(n) |> unnest_ft_meld(n) |> fst;

let rec height = (t: t) =>
  t.meld |> Option.map(height_meld) |> Option.value(~default=0)
and height_meld = (M(l, W((toks, _)), r)) =>
  toks
  |> List.map(Block.height)
  |> List.fold_left((+), height(l) + (+ height(r)));

let rec of_cell = (~delim=Delim.root, c: Cell.t): t =>
  Cell.get(c)
  |> Option.map(of_meld)
  |> Option.map((M(l, _, _) as m: meld) => {
       let indent = Delim.indent(delim) && height(l) > 0 ? 2 : 0;
       nest_body_meld(indent, m);
     })
  |> (meld => Cell.Base.mk(~meld?, ()))
and of_meld = (m: Meld.t): meld =>
  Meld.to_chain(m)
  |> Chain.fold_left_map(
       l => ((), of_cell(l)),
       ((), tok, cell) => {
         let b_tok = Block.of_tok(tok);
         let t_cell = of_cell(~delim=Node(tok), cell);
         ((), b_tok, t_cell);
       },
     )
  |> snd
  |> Meld.Base.of_chain;

let rec depad = (~side: Dir.t, t: t) =>
  switch (t.meld) {
  | None => (t, empty)
  | Some(M(_, W((bs, _)), _)) when List.for_all(Block.is_space, bs) => (
      t,
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
