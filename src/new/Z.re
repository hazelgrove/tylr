module Exp = {
  type t = option(t')
  and t' =
    | Op_z(op)
    | Pre_z(pre, HExp.t)
    | Pre_r(HExp.pre, t)
    | Post_z(HExp.t, post)
    | Post_l(t, HExp.post)
    | Bin_l(t, HExp.bin, HExp.t)
    | Bin_r(HExp.t, HExp.bin, t)
  and op =
    | Paren_body(t)
  and pre =
    | Let_def(HPat.t, t)
  and post =
    | Ap_arg(t);

  type descendant = [
    | `Exp(t)
    | `Pat(Pat.t)
  ];

  let move = (d: Direction.t, j: int, ts: HTile.s): option((int, HTile.s, descendant)) => {
    let+ (prefix, tile, suffix) =
      ListUtil.split_nth_opt(d == Left ? j - 1 : j, zipped);
    let moved_past = (d == Left ? j - 1 : j + 1, ts, None);
    let entered_j = ts =>
      d == Left ? List.length(ts) : 0;

  };
};