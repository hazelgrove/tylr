type shape =
  | Operand
  | PreOp
  | PostOp
  | BinOp;

module type S = {
  type t;
  let operand_hole: t;
  let operator_hole: t;
  let shape: t => shape;
  let precedence: t => int;
  let associativity: t => option(Associativity.t);
};
