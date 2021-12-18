[@deriving sexp]
type s = list(t)
and t = Identified.t(tokens)
and tokens = Aba.t(Token.t, s);

let get = Identified.get;

module Mold = {
  module Sorts = {
    type t = {
      out: Sort.t,
      in: list(Sort.t),
    };

    let of_tokens: list(Token.t) => list(t) =
      fun
      | ["let", "=", "in"] => [{out: Exp, in: [Pat, Exp]}]
      | _ => [];
  };

  type t = Mold.Base.t(Sorts.t);

  let num_tokens = ({sorts, _}: Mold.t) => List.length(sorts.in) + 1;
};

module Molds = {

}


module Frame = {
  type tiles = s;
  type tile = t;

  type t' = Aba.Frame.b(Token.t, tiles);
  type t = Identified.t(t');
  type s = ListFrame.t(tile);
};

let tip = d => get(Tile_pat.tip(d), Tile_exp.tip(d));

let mk_hole = id_gen => {
  let (id, id_gen) = IdGen.next(id_gen);
  fun
  | (Tip.Convex, Sort.Pat) => (Pat((id, OpHole)), id_gen)
  | (Concave, Pat) => (Pat((id, BinHole)), id_gen)
  | (Convex, Exp) => (Exp((id, OpHole)), id_gen)
  | (Concave, Exp) => (Exp((id, BinHole)), id_gen);
};

let precedence = get(Tile_pat.precedence, Tile_exp.precedence);
let associativity =
  get(_ => Tile_pat.associativity, _ => Tile_exp.associativity);

let precedence = (tile: Tile.t, mold: Mold.t) =>
  switch (tile, mold) {
  | (
      ("", []),
      {tips: {l: Convex, r: Convex}, _}
    ) => 0
  | (
      ("", []),
      {tips: {l: Concave, r: Concave}, _}
    ) => 0
  | ((t, []), _) when Token.is_var(t) || Token.is_num(t) => 0
  | (("!", []), _) => 1
  | (("[", [(_, "]")]), _) => 2
  | (("*" | "/", []), _) => 3
  | (("+" | "-", []), _) => 4
  }


let precedence: t => int =
  Identified.get(
    fun
    | Some(("!", [])) => 1
    | Some(("[", [(_, "]")])) => 2
    | Some(("*" | "/", [])) => 3
    | Some(("+" | "-", [])) => 4
    | Some(())

    | None => 0
    | Some(t') =>
      switch (t') {
      |
      }
    | Some((_, [])) => 0
    | Fact => 1
    | Ap(_) => 2
    | Times
    | Div => 3
    | Plus
    | Minus => 4
    | BinHole => 5
    | Prod => 6
    | Cond(_) => 7
    | Let(_) => 9,
  )