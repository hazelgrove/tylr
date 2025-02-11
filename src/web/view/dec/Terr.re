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
        ~state as s_init: L.State.t,
        ~eq,
        ~null,
        terr: LTerr.t,
      ) => {
    let ((state, wald_ns), wald) =
      W.Profile.mk(
        ~sil,
        ~whole,
        ~state=s_init,
        ~null=(null, Mtrl.is_space(LCell.sort(terr.cell))),
        ~eq=(eq, false),
        terr.wald,
      );

    let (p_r, cell) = LCell.depad(terr.cell, ~side=R);
    let ((state, cell_ns), cell) =
      Child.Profile.mk(
        ~sil,
        ~whole,
        ~ind=L.Indent.curr(s_init.ind),
        ~state,
        ~null=(false, true),
        cell,
      );

    let inner =
      sil
        ? [
          Silhouette.Inner.Profile.mk(
            ~is_space=Mtrl.is_space(LTerr.sort(terr)),
            Stds.Lists.consnoc_pairs(s_init, wald_ns @ cell_ns, state),
          ),
        ]
        : [];
    let p = {...wald, inner, cells: wald.cells @ [cell]};

    L.State.clear_log();
    let state = L.State.jump_cell(state, ~over=p_r);
    let p_r_ns = L.State.get_log();
    let ns = List.concat([wald_ns, cell_ns, p_r_ns]);

    ((state, ns), p);
  };

  let mk_r =
      (
        ~sil=false,
        ~whole: LCell.t,
        ~state as s_init: L.State.t,
        ~null,
        ~eq,
        terr: LTerr.t,
      ) => {
    let (p_l, cell) = LCell.depad(~side=L, terr.cell);
    L.State.clear_log();
    let s_post_p_l = L.State.jump_cell(s_init, ~over=p_l);
    let p_l_ns = L.State.get_log();

    // extraneous state calc to get token indentation for child profile
    let s_tok = L.State.jump_cell(s_post_p_l, ~over=cell);
    let ((state, cell_ns), cell) =
      Child.Profile.mk(
        ~sil,
        ~whole,
        ~ind=L.Indent.curr(s_tok.ind),
        ~state=s_post_p_l,
        ~null=(false, true),
        cell,
      );

    let ((state, wald_ns), wald) =
      W.Profile.mk(
        ~sil,
        ~whole,
        ~state,
        ~null=(Mtrl.is_space(LCell.sort(terr.cell)), null),
        ~eq=(false, eq),
        Wald.rev(terr.wald),
      );

    let inner =
      sil
        ? [
          Silhouette.Inner.Profile.mk(
            ~is_space=Mtrl.is_space(LTerr.sort(terr)),
            Stds.Lists.consnoc_pairs(s_post_p_l, cell_ns @ wald_ns, state),
          ),
        ]
        : [];

    let ns = List.concat([p_l_ns, cell_ns, wald_ns]);
    let p = {...wald, inner, cells: [cell] @ wald.cells};
    ((state, ns), p);
  };
};
