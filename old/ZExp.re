type t = (t', side)
and t' = (
  Skel.t,
  ZSeq.t(
    UHExp.operand, UHExp.pre_unop, UHExp.post_unop, UHExp.binop,
    zoperand, zpre, zpost, zbinop,
  )
)
and zoperand =
  | HoleZ
  | NumZ(int)
  | ParenZ_open(Exp.t)
  | ParenZ_body(t')
  | ParenZ_close(Exp.t)
and zpre =
  | IfZ_if(Exp.t, Exp.t)
  | IfZ_cond(t', Exp.t)
  | IfZ_then(Exp.t, Exp.t)
  | IfZ_then_clause(Exp.t, t')
  | IfZ_else(Exp.t, Exp.t)
and zpost = |
and zbinop =
  | PlusZ
  | TimesZ
  | EqZ;

type ztile = ZTile.t(
  zoperand, zpre_unop, zpost_unop, zbinop,
);
type ztiles = ZTile.s(
  UHExp.operand, UHExp.pre_unop, UHExp.post_unop, UHExp.binop,
  zoperand, zpre, zpost, zbinop,
);


/*
type zopseq('zoperand, 'zunop, 'zbinop) =
  (Skel.t, ZSeq.t(UHExp.operand, UHExp.unop, UHExp.binop, 'zoperand, 'zunop, 'zbinop))

type subject = zopseq(zoperand_subject, zunop_subject, zbinop_subject)
and zoperand_subject =
| HoleZ
| NumZ(int)
| ParenZ_open(Exp.t)
| ParenZ_close(Exp.t)
and zunop_subject =
| IfZ_if(Exp.t, Exp.t)
| IfZ_then(Exp.t, Exp.t)
| IfZ_else(Exp.t, Exp.t)
and zbinop_subject =
| PlusZ
| TimesZ
| EqZ;

type frame = zopseq(zoperand_frame, zunop_frame, zbinop_frame)
and zoperand_frame =
| ParenZ_body
and zunop_frame =
| IfZ_cond(Exp.t)
| IfZ_then_clause(Exp.t)
and zbinop_frame = |;

// top down
type t = {
  frames: list(frame),
  subject,
  cursor: side,
};

// 1 + (2 + ( 3 + 4| )) + 5
// frame: 1 + ( . ) + 5
// frame: 2 + ( . )
// subject: 3 + 4|
let seq = {
  frames: [
    ZOperand([], ParenZ_body, (A(Plus, Num(1)), A(Plus, Num(5)))),
    ZOperand([], ParenZ_body, (A(Plus, Num(2)), E)),
  ],
  subject: ZOperand([], NumZ(4), (A(Plus, Num(3)), E)),
  cursor: After,
};

// 1 + ( 2 + if 3 then 4 |else 5 )
let if = {
  frames: [
    ZOperand([], ParenZ_body, (A(Plus, Num(1)), E)),
  ],
  subject: ZUnOp(([], IfZ_else(Num(3), Num(4)), []), Num(5), (A(Plus, Num(2)), E)),
  cursor: Before,
};

type selection = {
  anchor: t,
  focus: t,
};

let rec zip_common_frames = (ze1: t, ze2: t): (list(frame), (t, t)) =>
  switch (ze1.frames, ze2.frames) {
  | ([f1, ...fs1], [f2, ...fs2]) when f1 == f2 =>
    let (common_frames, (ze1, ze2)) =
      split_common_frames({...ze1, frames: fs1}, {...ze2, frames: fs2});
    ([f1, ...common_frames], (ze1, ze2));
  | _ => ([], (ze1, ze2))
  };

type frames = list(frame);
type framed('a) = (frames, 'a);

let rec zip_common_frames = (frames1: frames, frames2: frames): (frames, (frames, frames)) =>
  switch (frames1, frames2) {
  | ([f1, ...fs1], [f2, ...fs2]) when f1 == f2 =>
    let (common_frames, (fs1, fs2)) =
      split_common_frames(fs1, fs2);
    ([f1, ...common_frames], (fs1, fs2));
  | _ => ([], (frames1, frames2))
  };

let split = ((_, zseq): subject, side): (list(UHExp.tile), list(UHExp.tile)) =>
  switch (zseq) {
  | ZOperand(unops, zoperand, surround) =>

  }

type frame_tile =
  | ZOperand(zoperand_frame)
  | ZUnOp(zunop_frame)
  | ZBinOp(zbinop_frame);
*/
// split_frame: frame => ZList.t(frame_tile, UHExp.tile)

