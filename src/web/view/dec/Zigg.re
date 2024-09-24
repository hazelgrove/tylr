module Tok = Token;
module T = Terr;
module W = Wald;
open Tylr_core;

module L = Layout;

module Profile = {
  type t = {
    up: list(T.Profile.t),
    top: W.Profile.t,
    dn: list(T.Profile.t),
  };

  let tokens = ({up, top, dn}: t) =>
    List.concat_map(T.Profile.tokens, up)
    @ fst(top)
    @ List.concat_map(T.Profile.tokens, dn);
  let cells = ({up, top, dn}: t) =>
    List.concat_map(T.Profile.cells, up)
    @ snd(top)
    @ List.concat_map(T.Profile.cells, dn);

  let mk =
      (
        ~whole: LCell.t,
        ~state: L.State.t,
        ~null: (bool, bool),
        ~eqs as (eqs_l, eqs_r): (list(int), list(int)),
        zigg: LZigg.t,
      ) => {
    let (up_len, dn_len) = List.(length(zigg.up), length(zigg.dn));
    let (state, up) =
      // reverse to get top-down index which matches eqs
      List.rev(zigg.up)
      |> List.mapi((i, t) => (i, t))
      // reverse back to go left to right
      |> List.rev
      |> List.fold_left_map(
           (state, (i, terr: LTerr.t)) => {
             let null = i == up_len - 1 && fst(null);
             let eq = List.mem(i, eqs_l);
             T.Profile.mk_l(~whole, ~state, ~eq, ~null, terr);
           },
           state,
         );
    let (state, top) = {
      let null = (
        zigg.up == [] ? fst(null) : false,
        zigg.dn == [] ? snd(null) : false,
      );
      let eq = List.(mem(-1, eqs_l), mem(-1, eqs_r));
      W.Profile.mk(~whole, ~state, ~null, ~eq, zigg.top);
    };
    let (_, dn) =
      // reverse to get top-down index which matches eqs
      List.rev(zigg.dn)
      |> List.mapi((i, t) => (i, t))
      |> List.fold_left_map(
           (state, (i, terr: LTerr.t)) => {
             let null = i == dn_len - 1 && snd(null);
             let eq = List.mem(i, eqs_r);
             T.Profile.mk_r(~whole, ~state, ~eq, ~null, terr);
           },
           state,
         );
    {up, top, dn};
  };
};

let mk = (~font, p: Profile.t) =>
  List.map(Tok.mk(~font), Profile.tokens(p))
  @ List.map(Child.mk(~font), Profile.cells(p));
