type t('op, 'pre, 'post, 'bin) =
  | Op('op)
  | Pre('pre, t('op, 'pre, 'post, 'bin) as 't)
  | Post('t, 'post)
  | Bin('t, 'bin, 't);

let get =
    (
      f_op: 'op => 'a,
      f_pre: ('pre, t('op, 'pre, 'post, 'bin) as 't) => 'a,
      f_post: ('t, 'post) => 'a,
      f_bin: ('t, 'bin, 't) => 'a,
      t: 't,
    ) =>
  switch (t) {
  | Op(op) => f_op(op)
  | Pre(pre, r) => f_pre(pre, r)
  | Post(l, post) => f_post(l, post)
  | Bin(l, bin, r) => f_bin(l, bin, r)
  };

module type S = {
  type op;
  type pre;
  type post;
  type bin;
  type nonrec t = t(op, pre, post, bin);

  let mk_op_hole: unit => op;
  let mk_bin_hole: unit => bin;

  let is_op_hole: op => bool;
  let is_bin_hole: bin => bool;
};
