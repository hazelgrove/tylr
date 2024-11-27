open Stds;

include Wald.Base;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Wald.Base.t(Block.t);

let sort = (w: t) => Block.sort(hd(w));

let flatten = (~flatten, W(w): t): Block.t =>
  // hcatting each block-wrapped cells with interleaved single-line tokens
  // will lead to a single block-wrapped element for whole wald
  w |> Chain.to_list(Fun.id, t => Block.wrap(flatten(t))) |> Block.hcats;

let unnest_ft = (~unnest_ft, n, W(w): t) =>
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
