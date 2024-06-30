open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Stds;

// todo: unify with existing structure by polymorphizing
[@deriving (show({with_path: false}), sexp, yojson)]
type t = option(meld)
and meld =
  | M(t, wald, t)
and wald =
  | W(Chain.t(Block.t, t));

let map: (_, t) => t = Option.map;

let rec flatten = (t: t): Block.t =>
  t |> Option.map(flatten_meld) |> Option.value(~default=Block.nil)
and flatten_meld = (M(l, w, r): meld): Block.t =>
  Block.hcats([flatten(l), flatten_wald(w), flatten(r)])
and flatten_wald = (W(w): wald) =>
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

let to_chain = (M(l, W(w), r): meld) => Chain.consnoc(l, w, r);
let of_chain = (c: Chain.t(t, Block.t)) => {
  let (l, w, r) = Result.get_exn(Invalid_argument(""), Chain.unconsnoc(c));
  M(l, W(w), r);
};

let rec end_path = (~side: Dir.t) =>
  fun
  | None => []
  | Some(M(_, _, r) as m) => [
      Chain.length(to_chain(m)) - 1,
      ...end_path(~side, r),
    ];

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
    : t
      |> Option.map(unnest_ft_meld(n))
      |> Option.map(Tuples.map_fst(Option.some))
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
  |> Tuples.map_fst(w => W(w));

let nest_body = (n: int, t: t) => t |> nest_tl(n) |> unnest_ft(n) |> fst;
let nest_body_meld = (n: int, m: meld) =>
  m |> nest_tl_meld(n) |> unnest_ft_meld(n) |> fst;

let rec height = (t: t) =>
  t |> Option.map(height_meld) |> Option.value(~default=0)
and height_meld = (M(l, _, r)) => height(l) + height(r);

let rec of_cell = (~delim=Delim.root, c: Cell.t): t =>
  Cell.get(c)
  |> Option.map(of_meld)
  |> Option.map((M(l, _, _) as m: meld) => {
       let indent = Delim.indent(delim) && height(l) > 0 ? 2 : 0;
       nest_body_meld(indent, m);
     })
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
  |> of_chain;
