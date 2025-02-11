// to keep a reference to token dec
module T = Token;
module W = Wald;
open Tylr_core;

// just for convenience
module L = Layout;

module Profile = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Layers.t;

  let mk = (~sil=false, ~whole: LCell.t, ~state: L.State.t, lm: LMeld.t) => {
    let M(lc_l, lw, lc_r) = lm;
    let (p_l, lc_l) = LCell.depad(~side=L, lc_l);
    let (p_r, lc_r) = LCell.depad(~side=R, lc_r);
    let lm = LMeld.mk(~l=lc_l, lw, ~r=lc_r);
    let null =
      Mtrl.(is_space(LCell.sort(lc_l)), is_space(LCell.sort(lc_r)));

    L.State.clear_log();
    let s_init = state |> L.State.jump_cell(~over=p_l);
    let p_l_ns = L.State.get_log();

    let s_tok = L.State.jump_cell(s_init, ~over=lc_l);

    let ((state, l_ns), l) =
      Child.Profile.mk(
        ~sil,
        ~whole,
        ~ind=L.Indent.curr(s_tok.ind),
        ~state=s_init,
        ~null=(true, false),
        lc_l,
      );
    let ((state, w_ns), w) =
      W.Profile.mk(~sil, ~whole, ~state, ~null, ~eq=(false, false), lw);
    let ((state, r_ns), r) =
      Child.Profile.mk(
        ~sil,
        ~whole,
        ~ind=L.Indent.curr(state.ind),
        ~state,
        ~null=(false, true),
        lc_r,
      );
    let inner =
      sil
        ? [
          Silhouette.Inner.Profile.mk(
            ~is_space=Mtrl.is_space(LMeld.sort(lm)),
            Stds.Lists.consnoc_pairs(
              s_init,
              List.concat([l_ns, w_ns, r_ns]),
              state,
            ),
          ),
        ]
        : [];

    L.State.clear_log();
    let final = state |> L.State.jump_cell(~over=p_r);
    let p_r_ns = L.State.get_log();

    let ns = List.concat([p_l_ns, l_ns, w_ns, r_ns, p_r_ns]);

    let p = {...w, inner, cells: [l] @ w.cells @ [r]};
    ((final, ns), p);
  };
};
