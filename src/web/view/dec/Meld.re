open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

// to keep a reference to token dec
module T = Token;
module W = Wald;
open Tylr_core;

// just for convenience
module L = Layout;

module Profile = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    chain: Chain.t(Child.Profile.t, T.Profile.t),
    sil: option(Silhouette.Inner.Profile.t),
  };
  exception No_tokens;

  let cells = p => Chain.loops(p.chain);
  let tokens = p => Chain.links(p.chain);

  let sort = (p: t) =>
    switch (tokens(p)) {
    | [] => raise(No_tokens)
    | [hd, ..._] => hd.style |> Option.map((style: T.Style.t) => style.sort)
    };

  let mk = (~sil=false, ~whole: LCell.t, ~state: L.State.t, lm: LMeld.t) => {
    let M(lc_l, lw, lc_r) = lm;
    let (p_l, lc_l) = LCell.depad(~side=L, lc_l);
    let (p_r, lc_r) = LCell.depad(~side=R, lc_r);
    let lm = LMeld.mk(~l=lc_l, lw, ~r=lc_r);
    let null =
      Mtrl.(is_space(LCell.sort(lc_l)), is_space(LCell.sort(lc_r)));

    let s_init = state |> L.State.jump_cell(~over=p_l);
    let s_tok = L.State.jump_cell(s_init, ~over=lc_l);

    let inner =
      sil
        ? [
          Silhouette.Inner.Profile.mk(
            ~is_space=Mtrl.is_space(LMeld.sort(lm)),
            ~state=s_init,
            LMeld.flatten(~flatten=LCell.flatten, lm),
          ),
        ]
        : [];

    let l =
      Child.Profile.mk(
        ~sil,
        ~whole,
        ~ind=L.Indent.curr(s_tok.ind),
        ~loc=s_init.loc,
        ~null=(true, false),
        lc_l,
      );
    let state = L.State.jump_cell(s_init, ~over=lc_l);
    let (state, w) =
      W.Profile.mk(~sil, ~whole, ~state, ~null, ~eq=(false, false), lw);
    let r =
      Child.Profile.mk(
        ~sil,
        ~whole,
        ~ind=L.Indent.curr(state.ind),
        ~loc=state.loc,
        ~null=(false, true),
        lc_r,
      );
    let state =
      state |> L.State.jump_cell(~over=lc_r) |> L.State.jump_cell(~over=p_r);
    let p = {...w, inner, cells: [l] @ w.cells @ [r]};
    // let p = {chain: Chain.consnoc(~hd=l, w, ~ft=r), sil: silhouette};
    (state, p);
  };
};
