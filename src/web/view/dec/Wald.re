module T = Token;
open Tylr_core;

// just for convenience
module L = Layout;

module Profile = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Chain.t(T.Profile.t, Child.Profile.t);

  let tokens = fst;
  let cells = snd;

  let mk =
      (
        ~whole,
        ~state: L.State.t,
        ~null: (bool, bool),
        ~eq: (bool, bool),
        W(w): LWald.t,
      ) => {
    let state = fst(eq) ? state : L.State.push_ind(state);
    let ind = L.Indent.curr(state.ind);
    // let ind = state.ind + state.rel;
    let n = Chain.length(w);
    // logic below assumes w won't be space
    w
    |> Chain.fold_left_map(
         b_tok => {
           let null = (fst(null), n == 1 && snd(null));
           let t = T.Profile.mk(~loc=state.loc, ~null, b_tok);
           let state = L.State.jump_tok(state, ~over=b_tok);
           (state, t);
         },
         (state, cell, b_tok) => {
           let c =
             Child.Profile.mk(
               ~whole,
               ~ind,
               ~loc=state.loc,
               ~null=(false, false),
               cell,
             );
           let state = L.State.jump_cell(state, ~over=cell);
           let null = (n == 1 && fst(null), snd(null));
           let t = T.Profile.mk(~loc=state.loc, ~null, b_tok);
           let state = L.State.jump_tok(state, ~over=b_tok);
           (state, c, t);
         },
       )
    |> Stds.Tuples.map_fst(snd(eq) ? Fun.id : L.State.pop_ind);
  };
};
