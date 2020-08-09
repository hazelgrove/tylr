module Term = {
  type t =
    | EHole
    | NHole(t)
    | Num(int)
    | Var(Var.t)
    | Paren(t)
    | If(t, t, t)
    | Let(HPat.Term.t, t, t)
    | Ann(t, HTyp.Term.t)
    | Plus(t, t)
    | Times(t, t)
    | Eq(t, t)
    | OHole(t, t);
};

module rec Tile: {
  type s = list(t)
  and t =
    | EHole
    | Num(int)
    | Var(Var.t)
    | Paren(s)
    | If(s, s)
    | Let(Pat.Tile.s, s)
    | Ann(Typ.Tile.s)
    | OHole
    | Plus
    | Times
    | Eq;

  type term = Term.t;

  let operand_hole: unit => t;
  let operator_hole: unit => t;

  let shape: t => TileShape.t(term);

  let roll: s => term;
  let unroll: term => s;
} = {
  type s = list(tile)
  and t =
    | EHole
    | Num(int)
    | Var(Var.t)
    | Paren(s)
    | If(s, s) // 1 + 2 + [if e1 then e2 else] (3 + 4)
    | Let(Pat.Tile.s, s)
    | Ann(Typ.Tile.s) /* 1 + [2 : Int -> Int] */
    | OHole
    | Plus
    | Times
    | Eq;

  let operand_hole = () => EHole;
  let operator_hole = () => OHole;

  let roll = TileParser.parse((module Tile));
  let rec unroll =
    fun
    | EHole => [EHole]
    | NHole(e) =>
      // TODO add err status to tiles
      unroll(e)
    | Num(n) => [Num(n)]
    | Var(x) => [Var(x)]
    | Paren(body) => [Paren(unroll(body))]
    | If(cond, then_, else_) => [
        If(unroll(cond), unroll(then_)),
        ...unroll(else_),
      ]
    | Let(p, def, body) => [
        Let(Pat.Tile.unroll(p), unroll(def)),
        ...unroll(body),
      ]
    | Ann(subj, ann) => unroll(ann) @ [Ann(Typ.Tile.unroll(ann))]
    | Plus(e1, e2) => unroll(e1) @ [Plus, ...unroll(e2)]
    | Times(e1, e2) => unroll(e1) @ [Times, ...unroll(e2)]
    | Eq(e1, e2) => unroll(e1) @ [Eq, ...unroll(e2)]
    | OHole(e1, e2) => unroll(e1) @ [OHole, ...unroll(e2)];

  let shape =
    fun
    | EHole => Operand(Term.EHole)
    | Num(n) => Operand(Term.Num(n))
    | Var(x) => Operand(Term.Var(x))
    | Paren(body) => Operand(Term.Paren(roll(body)))
    | If(cond, then_) =>
      PreOp(else_ => Term.If(roll(cond), roll(then_), else_), 10)
    | Let(p, def) =>
      PreOp(body => Term.Let(Pat.Term.roll(p), roll(def), body), 10)
    | Ann(ann) => PostOp(subj => Term.Ann(subj, Typ.Term.roll(ann)), 9)
    | Plus => BinOp((e1, e2) => Term.Plus(e1, e2), 3)
    | Times => BinOp((e1, e2) => Term.Times(e1, e2), 2, Left)
    | Eq => BinOp((e1, e2) => Term.Eq(e1, e2), 5, Left)
    | OHole => BinOp((e1, e2) => Term.Eq(e1, e2), 1, Left);
};
