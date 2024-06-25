// todo: unify with existing structure by polymorphizing
type t = option(meld)
and meld =
  | M(t, wald, t)
and wald =
  | W(Chain.t(Block.t, t));

let map: (_, t) => t = Option.map;

let to_chain = (M(l, W(w), r): meld) => Chain.consnoc(l, w, r);
let of_chain = c => {
  let (l, w, r)  = Option.get_exn(Invalid_argument(""), Chain.unconsnoc(c));
  M(l, W(w), r);
};

let rec flatten = (t: t): Block.t =>
  t
  |> map(flatten_meld)
  |> Option.value(~default=Block.nil)
and flatten_meld = (M(l, w, r): meld) =>
  Block.hcats([
    flatten(l),
    flatten_wald(w),
    flatten(r),
  ])
and flatten_wald = (W(w): wald) =>
  w
  |> Chain.map_link(t => Block.wrap(flatten(t)))
  |> Chain.flatten;

let rec nest_tl = (n: int) => map(n == 0 ? Fun.id : nest_tl_meld(n))
and nest_tl_meld = (n, M(l, W(w), r): meld) =>
  M(nest_tl(n, l), W(Chain.map_loop(Block.nest_tl(n), w)), nest_tl(n, r));

let rec unnest_ft = (n: int, t: t) =>
  n == 0
  ? (t, true)
  : t
    |> Option.map(unnest_ft_meld(n))
    |> Option.value(~default=(t, false))
and unnest_ft_meld = (n, M(l, W(w), r): meld) => {
  let (r, done) = unnest_ft(n, r);
  let (w, done) = (done ? Fun.id : unnest_ft_wald(n))(w);
  let (l, done) = (done ? Fun.id : unnest_ft(n))(l);
  (M(l, W(w), r), done);
}
and unnest_ft_wald = (n, W(w): wald) =>
  w
  |> Chain.fold_right_map(
    (b_tok, t, done) => {
      let (t, done) = (done ? Fun.id : unnest_ft(n))(r);
      let (b_tok, done) = (done ? Fun.id : Block.unnest_ft(n))(b_tok);
      (b_tok, t, done);
    },
    Block.unnest_ft,
  );

let nest_body = (n: int, t: t) =>
  t
  |> nest_tl(n)
  |> unnest_ft(n)
  |> fst;
let nest_body_meld = (n: int, m: meld) =>
  m
  |> nest_tl_meld(n)
  |> unnest_ft_meld(n)
  |> fst;

let rec height = (t: t) => t |> map(height_meld) |> Option.value(~default=0)
and height_meld  = (M(l, _, r)) => height(l) + height(r);

let rec of_cell = (~delim=Delim.root, c: Cell.t): t =>
  Cell.get(c)
  |> Option.map(of_meld(~delim))
  |> Option.map((M(l, _, _) as m: meld) => {
    let indent = Delim.indent(delim) && height(l) > 0 ? 2 : 0;
    nest_body_meld(indent, m);
  })
and of_meld = (M(l, w, r): Meld.t): meld =>
  Meld.to_chain(m)
  |> Chain.mapi_loop((i, cell) => (i, cell))
  |> Chain.fold_left_map(
      ((_, l)) => {
        let t = of_cell(l);
        ((), t);
      },
      ((), tok, (i, cell)) => {
        let b_tok = Block.of_tok(tok);
        let t_cell = of_cell(~delim=Node(tok), cell);
        ((), b_tok, t_cell);
      },
    )
  |> snd;

module Chain = {
  include Chain;
  type t = Chain.t(Tree.t, Block.t);
  let flatten = (c: Chain.t(t, t)) =>
    c
    |> Chain.fold_right(
         (t, b, acc) => Block.hcats([Tree.flatten(t), b, acc]),
         Tree.flatten,
       );
};

module Affix = {
  type t = Chain.Affix.t(Block.t, Tree.t);
  let flatten = (~side: Dir.t, aff: t) =>
    aff
    |> List.concat_map(((b, t)) => [b, Tree.flatten(t)])
    |> Dir.pick(side, (List.rev, Fun.id))
    |> Block.hcats;
};
