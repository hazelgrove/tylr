open Util;

type t = Chain.t(Rel.t(Cell.t, Cell.t), Token.t);

let fold =
    (
      f_c: (_, 'acc) => 'acc,
      f_tc: (Token.t, _, 'acc) => 'acc,
      init: 'acc,
      baked: t,
    ) => {
  let ((ts, cs), c) = Chain.split_ft(baked);
  let acc = List.fold_right2(f_tc, ts, cs, init);
  f_c(c, acc);
};

let is_eq = ((rels, toks): t) =>
  rels
  |> List.map(Rel.is_eq)
  |> OptUtil.sequence
  |> Option.map(cells => (cells, toks));
