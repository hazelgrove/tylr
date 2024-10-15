module Tok = Token;
module T = Terr;
module W = Wald;
open Tylr_core;

module L = Layout;

module Profile = {
  type t = {
    up: list((T.Profile.t, Silhouette.Profile.t)),
    top: (W.Profile.t, Silhouette.Profile.t),
    dn: list((T.Profile.t, Silhouette.Profile.t)),
  };

  let tokens = ({up, top, dn}: t) =>
    List.concat_map(((t, _)) => T.Profile.tokens(t), up)
    @ fst(fst(top))
    @ List.concat_map(((t, _)) => T.Profile.tokens(t), dn);
  let cells = ({up, top, dn}: t) =>
    List.concat_map(((t, _)) => T.Profile.cells(t), up)
    @ snd(fst(top))
    @ List.concat_map(((t, _)) => T.Profile.cells(t), dn);
  let silhouettes = ({up, top, dn}: t) =>
    List.map(snd, up) @ [snd(top)] @ List.map(snd, dn);

  let mk =
      (
        ~whole: LCell.t,
        ~state: L.State.t,
        ~null: (bool, bool),
        // indices into the slopes indicating which have delim-matching counterparts
        // in the surrounding context. indices assume slopes in top-down order. -1
        // is used to indicate that the top wald matches
        ~eqs as (eqs_l, eqs_r): (list(int), list(int)),
        zigg: LZigg.t,
      ) => {
    let (up_len, dn_len) = List.(length(zigg.up), length(zigg.dn));
    let l_bound =
      switch (zigg.up) {
      | [t, ..._] when Mtrl.is_space(LTerr.sort(t)) => up_len - 2
      | _ => up_len - 1
      };
    let r_bound =
      switch (zigg.dn) {
      | [t, ..._] when Mtrl.is_space(LTerr.sort(t)) => dn_len - 2
      | _ => dn_len - 1
      };
    let (state, up) =
      // reverse to get top-down index which matches eqs
      List.rev(zigg.up)
      |> List.mapi((i, t) => (i, t))
      // reverse back to go left to right
      |> List.rev
      |> List.fold_left_map(
           (state, (i, terr: LTerr.t)) => {
             let null = i >= l_bound && fst(null);
             let eq = List.mem(i, eqs_l);
             let b = LTerr.flatten(terr);
             let sil = Silhouette.Profile.mk(~style=Inner, ~state, b);
             let (state, p) =
               T.Profile.mk_l(~whole, ~state, ~eq, ~null, terr);
             (state, (p, sil));
           },
           state,
         );
    let (state, top) = {
      let null = (
        List.for_all(t => Mtrl.is_space(LTerr.sort(t)), zigg.up)
        && fst(null),
        List.for_all(t => Mtrl.is_space(LTerr.sort(t)), zigg.dn)
        && snd(null),
      );
      let eq = List.(mem(-1, eqs_l), mem(-1, eqs_r));
      let sil =
        Silhouette.Profile.mk(
          ~style=Inner,
          ~state,
          LWald.flatten(~flatten=LCell.flatten, zigg.top),
        );
      let (state, p) = W.Profile.mk(~whole, ~state, ~null, ~eq, zigg.top);
      (state, (p, sil));
    };
    let (_, dn) =
      // reverse to get top-down index which matches eqs
      List.rev(zigg.dn)
      |> List.mapi((i, t) => (i, t))
      |> List.fold_left_map(
           (state, (i, terr: LTerr.t)) => {
             let null = i >= r_bound && snd(null);
             let eq = List.mem(i, eqs_r);
             let b = LTerr.flatten(terr);
             let sil = Silhouette.Profile.mk(~style=Inner, ~state, b);
             let (state, p) =
               T.Profile.mk_r(~whole, ~state, ~eq, ~null, terr);
             (state, (p, sil));
           },
           state,
         );
    {up, top, dn};
  };
};

let mk = (~font, p: Profile.t) =>
  List.map(Silhouette.mk(~font), Profile.silhouettes(p))
  @ List.map(Tok.mk(~font), Profile.tokens(p))
  @ List.map(Child.mk(~font), Profile.cells(p));
