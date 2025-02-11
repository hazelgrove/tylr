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
        ~state as init: L.State.t,
        ~null: (bool, bool),
        ~eq: (bool, bool),
        W(w) as lw: LWald.t,
      ) => {
    L.State.clear_log();
    let is_space = Mtrl.is_space(LWald.sort(lw));
    let state = (fst(eq) || is_space ? Fun.id : L.State.push_ind)(init);
    let ind = L.Indent.curr(state.ind);
    let n = Chain.length(w);
    let (state, (tokens, cells)) =
      w
      |> Chain.mapi_loop((i, b) => (i, b))
      |> Chain.fold_left_map(
           ((_, b_tok)) => {
             let null = (fst(null), n == 1 && snd(null));
             let ((state, tok_log), t) =
               T.Profile.mk(~sil, ~state, ~null, b_tok);
             (state, (t, tok_log));
           },
           (state, cell, (i, b_tok)) => {
             let ((state, cell_ns), c) =
               Child.Profile.mk(
                 ~sil,
                 ~whole,
                 ~ind,
                 ~state,
                 ~null=(false, false),
                 cell,
               );
             let null = (false, i == n - 1 && snd(null));
             let ((state, tok_ns), t) =
               T.Profile.mk(~sil, ~state, ~null, b_tok);
             (state, (c, cell_ns), (t, tok_ns));
           },
         )
      |> Stds.Tuples.map_fst(snd(eq) || is_space ? Fun.id : L.State.pop_ind);
    let (ts, ts_ns) = List.split(tokens);
    let (cs, cs_ns) = List.split(cells);
    let ns =
      Chain.mk(ts_ns, cs_ns) |> Chain.to_list(Fun.id, Fun.id) |> List.concat;
    let inner =
      sil
        ? [
          Silhouette.Inner.Profile.mk(
            ~is_space=Mtrl.is_space(LWald.sort(lw)),
            Stds.Lists.consnoc_pairs(init, ns, state),
          ),
        ]
        : [];
    ((state, ns), Layers.mk(~inner, ~cells=cs, ~tokens=ts, ()));
  };
};
