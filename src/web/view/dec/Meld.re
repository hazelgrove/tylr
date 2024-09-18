// to keep a reference to token dec
module T = Token;
module W = Wald;
open Tylr_core;

// just for convenience
module L = Layout;

module Profile = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Chain.t(Child.Profile.t, T.Profile.t);
  exception No_tokens;

  let cells = p => Chain.loops(p);
  let tokens = p => Chain.links(p);

  let sort = (p: t) =>
    switch (tokens(p)) {
    | [] => raise(No_tokens)
    | [hd, ..._] => hd.style |> Option.map((style: T.Style.t) => style.sort)
    };

  let mk = (~whole: LCell.t, ~state: L.State.t, lm: LMeld.t) => {
    let M(lc_l, lw, lc_r) = lm;
    let (p_l, lc_l) = LCell.depad(~side=L, lc_l);
    let (_, lc_r) = LCell.depad(~side=R, lc_r);
    let null =
      Mtrl.(is_space(LCell.sort(lc_l)), is_space(LCell.sort(lc_r)));

    let s_init = state |> L.State.jump_cell(~over=p_l);
    let s_tok = L.State.jump_cell(s_init, ~over=lc_l);

    let l =
      Child.Profile.mk(
        ~whole,
        ~ind=s_tok.ind,
        ~loc=s_init.loc,
        ~null=(true, false),
        lc_l,
      );
    let state = L.State.jump_cell(s_tok, ~over=lc_l);
    let (state, w) = W.Profile.mk(~whole, ~state, ~null, lw);
    let r =
      Child.Profile.mk(
        ~whole,
        ~ind=s_tok.ind,
        ~loc=state.loc,
        ~null=(false, true),
        lc_r,
      );
    Chain.consnoc(~hd=l, w, ~ft=r);
  };
};

let mk = (~font, p: Profile.t) =>
  List.map(T.mk(~font), Profile.tokens(p))
  @ List.map(Child.mk(~font), Profile.cells(p));
