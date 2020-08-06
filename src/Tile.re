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

let fix_empty_holes =
    (type a, module Tile: S with type t = a, tiles: list(a)): list(a) => {
  let rec fix_operand = (tiles: list(Tile.t)) =>
    switch (tiles) {
    | [] => [Tile.operand_hole]
    | [t, ...ts] =>
      switch (Tile.shape(t)) {
      | PreOp => [t, ...fix_operand(ts)]
      | Operand => [t, ...fix_operator(ts)]
      | PostOp
      | BinOp => [Tile.operand_hole, ...fix_operator(tiles)]
      }
    }
  and fix_operator = (tiles: list(Tile.t)) =>
    switch (tiles) {
    | [] => []
    | [t, ...ts] =>
      switch (Tile.shape(t)) {
      | PostOp => [t, ...fix_operator(ts)]
      | BinOp => [t, ...fix_operand(ts)]
      | PreOp
      | Operand => [Tile.operator_hole, ...fix_operand(tiles)]
      }
    };
  fix_operand(tiles);
};