type t('operand, 'pre, 'post, 'binop, 'zoperand, 'zpre, 'zpost, 'zbinop) =
| ZOperand(
    'zoperand,
    Seq.affix_affix('operand, 'pre, 'post, 'binop),
  )
| ZPre(
    'zpre,
    Seq.affix_seq('operand, 'pre, 'post, 'binop),
  )
| ZPost(
    'zpost,
    Seq.seq_affix('operand, 'pre, 'post, 'binop),
  )
| ZBinOp(
    'zbinop,
    Seq.seq_seq('operand, 'pre, 'post, 'binop),
  );

let mk_ztiles =
  fun
  | ZOperand(zoperand, (prefix, suffix)) =>
    (
      Seq.tiles_of_affix(prefix),
      ZTile.ZOperand(zoperand),
      Seq.tiles_of_affix(suffix),
    )
  | ZPre(zpre, (prefix, suffix)) =>
    (
      Seq.tiles_of_affix(prefix),
      ZTile.ZPre(zpre),
      Seq.tiles_of_seq(suffix),
    )
  | ZPost(zpost, (prefix, suffix)) =>
    (
      Seq.tiles_of_seq(prefix),
      ZTile.ZPost(zpost),
      Seq.tiles_of_affix(suffix),
    )
  | ZBinOp(zbinop, (prefix, suffix)) =>
    (
      Seq.tiles_of_seq(prefix),
      ZTile.ZBinOp(zbinop),
      Seq.tiles_of_seq(suffix),
    );

let split_seq = (
  n: int,
  seq: Seq.t('operand, 'pre, 'post, 'binop),
): option(t('operand, 'pre, 'post, 'binop, 'operand, 'pre, 'post, 'binop)) => {
  switch (n, seq) {
  | _ when n < 0 => failwith("seq index out of bounds")
  | (0, Pre(pre, suffix)) => ZPre(pre, (Nil, suffix))
  | (_, Pre(pre, suffix)) =>
    switch (split_seq(n - 1, suffix)) {
    | ZOperand(zoperand,)
    }
  | (0, Operand(operand, suffix)) => ZOperand(operand, (Nil, suffix))
  }


  if (n < 0) {
    None
  } else if (n == 0) {
    switch (seq) {
    | Pre(pre, suffix) => Some()
    | Operand(operand, suffix) => Some()
    }
  } else {
    switch (seq) {
    | Pre(pre, seq) =>
    }
  }
  switch (n, seq) {
  | (0, Pre(pre, suffix)) =>
    Some(ZPre(pre, (Nil, suffix)))
  | (0, Operand())
  }
}