type t =
  | EHole
  | NHole(t)
  | Num(int)
  | Var(Var.t)
  | Paren(t)
  | If(t, t, t)
  | Let(HPat.t, t, t)
  | Ann(t, HTyp.t)
  | Plus(t, t)
  | Times(t, t)
  | Eq(t, t)
  | OHole(t, t);

module Tile = {
  type term = t;
  type t =
    | EHole
    | Num(int)
    | Var(Var.t)
    | Paren(term)
    | If(term, term)
    | Let(HPat.t, term)
    | Ann(HTyp.t)
    | OHole
    | Plus
    | Times
    | Eq;

  let mk_operand_hole = () => EHole;
  let mk_operator_hole = () => OHole;

  let is_operand_hole = (==)(EHole);
  let is_operator_hole = (==)(OHole);

  let shape: t => TileShape.t(term) =
    fun
    | EHole => Operand(EHole)
    | Num(n) => Operand(Num(n))
    | Var(x) => Operand(Var(x))
    | Paren(body) => Operand(Paren(body))
    | If(cond, then_) => PreOp(else_ => If(cond, then_, else_), 10)
    | Let(p, def) => PreOp(body => Let(p, def, body), 10)
    | Ann(ann) => PostOp(subj => Ann(subj, ann), 9)
    | Plus => BinOp((e1, e2) => Plus(e1, e2), 3, Left)
    | Times => BinOp((e1, e2) => Times(e1, e2), 2, Left)
    | Eq => BinOp((e1, e2) => Eq(e1, e2), 5, Left)
    | OHole => BinOp((e1, e2) => Eq(e1, e2), 1, Left);
};

let fix_empty_holes = TileParser.fix_empty_holes((module Tile));
let parse = TileParser.parse((module Tile));
let rec unparse: t => list(Tile.t) =
  fun
  | EHole => [EHole]
  | NHole(e) =>
    // TODO add err status to tiles
    unparse(e)
  | Num(n) => [Num(n)]
  | Var(x) => [Var(x)]
  | Paren(body) => [Paren(body)]
  | If(cond, then_, else_) => [If(cond, then_), ...unparse(else_)]
  | Let(p, def, body) => [Let(p, def), ...unparse(body)]
  | Ann(subj, ann) => unparse(subj) @ [Ann(ann)]
  | Plus(e1, e2) => unparse(e1) @ [Plus, ...unparse(e2)]
  | Times(e1, e2) => unparse(e1) @ [Times, ...unparse(e2)]
  | Eq(e1, e2) => unparse(e1) @ [Eq, ...unparse(e2)]
  | OHole(e1, e2) => unparse(e1) @ [OHole, ...unparse(e2)];
