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

  let mk_l =
      (
        ~sil=false,
        ~whole: LCell.t,
        ~state: L.State.t,
        ~eq,
        ~null,
        terr: LTerr.t,
      ) => {
    let ind = L.Indent.curr(state.ind);
    // let (null_l, null_r) = ;
    // let state = eq ? state : L.State.push_ind(state);
    let inner =
      sil
        ? [
          Silhouette.Inner.Profile.mk(
            ~is_space=Mtrl.is_space(LTerr.sort(terr)),
            ~state,
            LTerr.L.flatten(terr),
          ),
        ]
        : [];
    let (state, wald) =
      W.Profile.mk(
        ~sil,
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
    let p = {...wald, inner, cells: wald.cells @ [cell]};
    (state, p);
  };

  let mk_r =
      (
        ~sil=false,
        ~whole: LCell.t,
        ~state: L.State.t,
        ~null,
        ~eq,
        terr: LTerr.t,
      ) => {
    let inner =
      sil
        ? [
          Silhouette.Inner.Profile.mk(
            ~is_space=Mtrl.is_space(LTerr.sort(terr)),
            ~state,
            LTerr.R.flatten(terr),
          ),
        ]
        : [];

    let s_mid = L.State.jump_cell(state, ~over=terr.cell);
    let cell =
      Child.Profile.mk(
        ~sil,
        ~whole,
        ~ind=L.Indent.curr(s_mid.ind),
        ~loc=state.loc,
        ~null=(false, true),
        terr.cell,
      );
    let (state, wald) =
      W.Profile.mk(
        ~sil,
        ~whole,
        ~state=s_mid,
        ~null=(Mtrl.is_space(LCell.sort(terr.cell)), null),
        ~eq=(false, eq),
        Wald.rev(terr.wald),
      );
    let p = {...wald, inner, cells: [cell] @ wald.cells};
    (state, p);
  };
};
