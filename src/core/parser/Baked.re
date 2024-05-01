open Util;

type t = Chain.t(Rel.t(Cell.t, Cell.t), Token.t);

let fold =
    (
      init: 'acc,
      f_ct: ('acc, _, Token.t) => 'acc,
      f_c: ('acc, _) => 'acc,
      baked: t,
    ) => {
  let ((ts, cs), c) = Chain.split_ft(baked);
  let acc = List.fold_right2((t, c, acc) => f_ct(acc, c, t), ts, cs, init);
  f_c(acc, c);
};

let is_eq = ((rels, toks): t) =>
  rels
  |> List.map(Rel.is_eq)
  |> OptUtil.sequence
  |> Option.map(cells => (cells, toks));
