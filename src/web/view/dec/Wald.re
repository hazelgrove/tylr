module T = Token;
open Tylr_core;

// just for convenience
module L = Layout;

module Profile = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Layers.t;

  let tokens = fst;
  let cells = snd;

  let mk =
      (
        ~sil=false,
        ~whole,
        ~state: L.State.t,
        ~null: (bool, bool),
        ~eq: (bool, bool),
        W(w) as lw: LWald.t,
      ) => {
    let state = fst(eq) ? L.State.pop_ind(state) : L.State.push_ind(state);
    let ind = L.Indent.curr(state.ind);
    // let ind = state.ind + state.rel;
    let n = Chain.length(w);
    let inner =
      sil
        ? [
          Silhouette.Inner.Profile.mk(
            ~is_space=Mtrl.is_space(LWald.sort(lw)),
            ~state,
            LWald.flatten(~flatten=LCell.flatten, lw),
          ),
        ]
        : [];
    // logic below assumes w won't be space
    let (state, (tokens, cells)) =
      w
      |> Chain.mapi_loop((i, b) => (i, b))
      |> Chain.fold_left_map(
           ((_, b_tok)) => {
             let null = (fst(null), n == 1 && snd(null));
             let t = T.Profile.mk(~sil, ~loc=state.loc, ~null, b_tok);
             let state = L.State.jump_tok(state, ~over=b_tok);
             (state, t);
           },
           (state, cell, (i, b_tok)) => {
             let c =
               Child.Profile.mk(
                 ~sil,
                 ~whole,
                 ~ind,
                 ~loc=state.loc,
                 ~null=(false, false),
                 cell,
               );
             let state = L.State.jump_cell(state, ~over=cell);
             let null = (false, i == n - 1 && snd(null));
             let t = T.Profile.mk(~sil, ~loc=state.loc, ~null, b_tok);
             let state = L.State.jump_tok(state, ~over=b_tok);
             (state, c, t);
           },
         )
      |> Stds.Tuples.map_fst(snd(eq) ? Fun.id : L.State.pop_ind);
    (state, Layers.mk(~inner, ~cells, ~tokens, ()));
  };
};
