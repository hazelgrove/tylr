module type S = {
  module Term: HTerm.S;
  module Frame: HFrame.S;

  module Subject: {
    // 1 | + (2 _ 3): {prefix: [1], z: (), suffix: [+, (2 _ 3)]}
    type pointing = ZList.t(unit, Term.tile);
    // 1 + [(] 2 _ 3 )
    // {prefix: [L(1), L(+)], z: (L, [(])), suffix: [L(2), L(_), L(3), R(`)`)]}
    type selecting =
      ZList.t((Direction.t, HSelection.t), Either.t(Term.tile, HTessera.t));
    // 1 + [( 2 _] 3 ) + 4
    // {prefix: [L(1), L(+)], z: (L, [[`(`]]), suffix: [L(2), L(_), L(3), R(`)`)]}
    type restructuring =
      ZList.t(
        (Direction.t, list(HSelection.t)),
        Either.t(Term.tile, HSelection.t),
      );

    type t =
      | Pointing(pointing)
      | Selecting(selecting)
      | Restructuring(restructuring);
  };

  type t = (Subject.t, Frame.bidelimited);
};

module Make = (Term: HTerm.S, Frame: HFrame.S) => {
  module Subject = {
    type pointing = ZList.t(unit, Term.tile);
    type selecting =
      ZList.t((Direction.t, HSelection.t), Either.t(Term.tile, HTessera.t));
    type restructuring =
      ZList.t(
        (Direction.t, list(HSelection.t)),
        Either.t(Term.tile, HSelection.t),
      );
    type t =
      | Pointing(pointing)
      | Selecting(selecting)
      | Restructuring(restructuring);
  };

  type t = (Subject.t, Frame.bidelimited);
};

module Typ = Make(HTyp, HTypFrame);
module Pat = Make(HPat, HPatFrame);
module Exp = Make(HExp, HExpFrame);

type t =
  | Typ(Typ.t)
  | Pat(Pat.t)
  | Exp(Exp.t);
