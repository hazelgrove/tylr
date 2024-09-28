module T = Token;
module W = Wald;
open Tylr_core;

module L = Layout;

module Profile = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    cell: Child.Profile.t,
    wald: W.Profile.t,
  };

  let tokens = (p: t) => W.Profile.tokens(p.wald);
  let cells = (p: t) => [p.cell, ...W.Profile.cells(p.wald)];

  let mk_l = (~whole: LCell.t, ~state: L.State.t, ~eq, ~null, terr: LTerr.t) => {
    let ind = L.Indent.curr(state.ind);
    // let (null_l, null_r) = ;
    // let state = eq ? state : L.State.push_ind(state);
    let (state, wald) =
      W.Profile.mk(
        ~whole,
        ~state,
        ~null=(null, Mtrl.is_space(LCell.sort(terr.cell))),
        ~eq=(eq, false),
        terr.wald,
      );
    let cell =
      Child.Profile.mk(
        ~whole,
        ~ind,
        ~loc=state.loc,
        ~null=(false, true),
        terr.cell,
      );
    let state = L.State.jump_cell(state, ~over=terr.cell);
    (state, {cell, wald});
  };

  let mk_r = (~whole: LCell.t, ~state: L.State.t, ~null, ~eq, terr: LTerr.t) => {
    let s_mid = L.State.jump_cell(state, ~over=terr.cell);
    let cell =
      Child.Profile.mk(
        ~whole,
        ~ind=L.Indent.curr(s_mid.ind),
        ~loc=state.loc,
        ~null=(false, true),
        terr.cell,
      );
    let (state, wald) =
      W.Profile.mk(
        ~whole,
        ~state=s_mid,
        ~null=(Mtrl.is_space(LCell.sort(terr.cell)), null),
        ~eq=(false, eq),
        Wald.rev(terr.wald),
      );
    (state, {cell, wald});
  };
};

let mk = (~font, p: Profile.t) =>
  List.map(T.mk(~font), Profile.tokens(p))
  @ List.map(Child.mk(~font), Profile.cells(p));
