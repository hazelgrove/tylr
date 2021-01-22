type t = option(t')
and t' =
  | TypeIncon(t)
  | Op_z(op)
  | Pre_z(pre, HExp.t)
  | Pre_r(HExp.pre, t)
  | Post_z(HExp.t, post)
  | Post_l(t, HExp.post)
  | Bin_l(t, HExp.bin, HExp.t)
  | Bin_z(HExp.t, bin, HExp.t)
  | Bin_r(HExp.t, HExp.bin, t)
and op =
  | Paren_body(t)
and pre =
  | Let_def(HPat.t, t)
and post =
  | Ap_arg(t)
and bin = unit; // empty

let enter = (d: Direction.t, j: int, ts: HTile.s): option((HTile.s, ))

