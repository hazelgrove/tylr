type t =
  | OperandHole
  | Num(HoleStatus.t, int)
  | Var(HoleStatus.t, Var.t)
  | Paren(t)
  | Lam(HoleStatus.t, HPat.t, t)
  | Plus(HoleStatus.t, t, t)
  | Ap(HoleStatus.t, t, t) // f(x)
  | OperatorHole(t, t);

let rec set_hole_status = (status, e) =>
  switch (e) {
  | OperandHole
  | OperatorHole(_) => e
  | Num(_, n) => Num(status, n)
  | Var(_, x) => Var(status, x)
  | Paren(body) => Paren(set_hole_status(status, body))
  | Lam(_, p, body) => Lam(status, p, body)
  | Plus(_, e1, e2) => Plus(status, e1, e2)
  | Ap(_, e1, e2) => Ap(status, e1, e2)
  };

module Tile = {
  type term = t;
  type t =
    // operand
    | OperandHole
    | Num(HoleStatus.t, int)
    | Var(HoleStatus.t, Var.t)
    | Paren(term)
    // pre
    | Lam(HoleStatus.t, HPat.t)
    // post
    | Ap(HoleStatus.t, term)
    // bin
    | OperatorHole
    | Plus(HoleStatus.t);

  let mk_operand_hole = () => OperandHole;
  let mk_operator_hole = () => OperatorHole;

  let is_operand_hole = (==)(OperandHole);
  let is_operator_hole = (==)(OperatorHole);

  let shape: t => TileShape.t(term) =
    fun
    | OperandHole => Operand(OperandHole)
    | Num(status, n) => Operand(Num(status, n))
    | Var(status, x) => Operand(Var(status, x))
    | Paren(body) => Operand(Paren(body))
    | Lam(status, p) => PreOp(body => Lam(status, p, body), 10)
    | Ap(status, arg) => PostOp(fn => Ap(status, fn, arg), 1)
    | Plus(status) => BinOp((e1, e2) => Plus(status, e1, e2), 3, Left)
    | OperatorHole => BinOp((e1, e2) => OperatorHole(e1, e2), 2, Left);

  let get_open_children =
    fun
    | OperandHole
    | Num(_)
    | Var(_)
    | Lam(_)
    | Plus(_)
    | OperatorHole => []
    | Paren(body) => [body]
    | Ap(_, arg) => [arg];

  let rec unparse: term => list(t) =
    fun
    | OperandHole => [OperandHole]
    | Num(status, n) => [Num(status, n)]
    | Var(status, x) => [Var(status, x)]
    | Paren(body) => [Paren(body)]
    | Lam(status, p, body) => [Lam(status, p), ...unparse(body)]
    | Ap(status, fn, arg) => unparse(fn) @ [Ap(status, arg)]
    | Plus(status, e1, e2) => unparse(e1) @ [Plus(status), ...unparse(e2)]
    | OperatorHole(e1, e2) => unparse(e1) @ [OperatorHole, ...unparse(e2)];

  let index_of_root: term => int =
    fun
    | OperandHole
    | Num(_)
    | Var(_)
    | Paren(_)
    | Lam(_) => 0
    | Ap(_, e, _)
    | Plus(_, e, _)
    | OperatorHole(e, _) => List.length(unparse(e));

  let set_hole_status = (status, tile) =>
    switch (tile) {
    | OperandHole
    | OperatorHole => tile
    | Num(_, n) => Num(status, n)
    | Var(_, x) => Var(status, x)
    | Paren(body) => Paren(set_hole_status(status, body))
    | Lam(_, p) => Lam(status, p)
    | Ap(_, arg) => Ap(status, arg)
    | Plus(_) => Plus(status)
    };
};
include TileUtil.Make(Tile);

let unparse = Tile.unparse;
