module T = Token;
open Tylr_core;

module L = Layout;

module Profile = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Chain.Affix.t(T.Profile.t, Child.Profile.t);

  let tokens = fst;
  let cells = snd;

  let mk_l = (~whole: LCell.t, ~state: L.State.t, ~null, terr: LTerr.t) => {
    let ind = state.ind;
    let (null_l, null_r) = (null, Mtrl.is_space(LCell.sort(terr.cell)));
    let (ts, cs) = LTerr.unmk(terr);
    let n = List.length(ts);
    List.combine(ts, cs)
    |> List.mapi((i, tc) => (i, tc))
    |> List.fold_left_map(
         (state: L.State.t, (i, (b_tok, cell))) => {
           let null = (i == 0 && null_l, i == n - 1 && null_r);
           let t = T.Profile.mk(~loc=state.loc, ~null, b_tok);
           let state = L.State.jump_tok(state, ~over=b_tok);
           let c =
             Child.Profile.mk(
               ~whole,
               ~ind,
               ~loc=state.loc,
               ~null=(false, i == n - 1),
               cell,
             );
           let state = L.State.jump_cell(state, ~over=cell);
           (state, (t, c));
         },
         state,
       )
    |> Stds.Tuples.map_snd(List.split);
  };

  let mk_r = (~whole: LCell.t, ~state: L.State.t, ~null, terr: LTerr.t) => {
    let ind = L.State.jump_cell(state, ~over=terr.cell).ind;
    let (null_l, null_r) = (Mtrl.is_space(LCell.sort(terr.cell)), null);
    let (ts, cs) = LTerr.unmk(terr);
    let n = List.length(ts);
    List.combine(cs, ts)
    |> List.rev
    |> List.mapi((i, ct) => (i, ct))
    |> List.fold_left_map(
         (state: L.State.t, (i, (cell, b_tok))) => {
           let c =
             Child.Profile.mk(
               ~whole,
               ~ind,
               ~loc=state.loc,
               ~null=(i == 0, false),
               cell,
             );
           let state = L.State.jump_cell(state, ~over=cell);
           let null = (i == 0 && null_l, i == n - 1 && null_r);
           let t = T.Profile.mk(~loc=state.loc, ~null, b_tok);
           let state = L.State.jump_tok(state, ~over=b_tok);
           (state, (t, c));
         },
         state,
       )
    |> Stds.Tuples.map_snd(List.rev)
    |> Stds.Tuples.map_snd(List.split);
  };
};

let mk = (~font, p: Profile.t) =>
  List.map(T.mk(~font), Profile.tokens(p))
  @ List.map(Child.mk(~font), Profile.cells(p));
