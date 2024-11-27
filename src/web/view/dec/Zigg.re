open Stds;

module Tok = Token;
module T = Terr;
module W = Wald;
module M = Meld;
open Tylr_core;

module L = Layout;

module Profile = {
  type t = Layers.t;

  let mk_rolled =
      (~side: Dir.t, ~whole, ~state, ~rel: Either.t(_), rolled: LCell.t) => {
    switch (rolled.meld) {
    | None => (state, Layers.empty)
    | Some(m) =>
      switch (rel) {
      | Left () => M.Profile.mk(~sil=true, ~whole, ~state, m)
      | Right((s_neighbor, neighbor)) =>
        let s_post = L.State.jump_cell(state, ~over=rolled);
        let s_tok = Dir.pick(side, (s_post, state));
        let sil =
          Silhouette.Inner.Profile.mk(
            ~is_space=Mtrl.is_space(LMeld.sort(m)),
            ~state=Dir.pick(side, (state, s_neighbor)),
            // merge this silhouette with the neighbor silhouette in rest of
            // unrolled zigg
            (LMeld.flatten(~flatten=LCell.flatten, m), neighbor)
            |> Dir.order(side)
            |> Funs.uncurry(Block.hcat),
          );
        let cell =
          Child.Profile.mk(
            ~sil=true,
            ~whole,
            ~ind=L.Indent.curr(s_tok.ind),
            ~loc=state.loc,
            ~null=Dir.pick(side, ((true, false), (false, true))),
            rolled,
          );
        let p = Layers.mk(~inner=[sil], ~cells=[cell], ());
        (s_post, p);
      }
    };
  };

  let mk_up = (~whole, ~state, ~null, ~eqs, ~unrolled: LSlope.t, up: LSlope.t) => {
    let l_bound =
      switch (unrolled) {
      | [t, ..._] when Mtrl.is_space(LTerr.sort(t)) =>
        LSlope.height(unrolled) - 2
      | _ => LSlope.height(unrolled) - 1
      };
    // reverse to get top-down index which matches eqs
    List.rev(up)
    |> List.mapi((i, t) => (i, t))
    // drop rolled terraces
    // |> Lists.take_while(~f=((i, _)) => i < up_len - fst(rolled))
    // reverse back to go left to right
    |> List.rev
    |> List.fold_left_map(
         (state, (i, terr: LTerr.t)) => {
           let null = i >= l_bound && null;
           let eq = List.mem(i, eqs);

           let (p_r, cell) = LCell.depad(terr.cell, ~side=R);
           let terr = {...terr, cell};

           let (state, p) =
             T.Profile.mk_l(~sil=true, ~whole, ~state, ~eq, ~null, terr);

           let state = L.State.jump_cell(state, ~over=p_r);

           (state, p);
         },
         state,
       )
    |> Tuples.map_snd(Layers.concat);
  };

  let mk_dn =
      (~whole, ~state, ~null, ~eqs, ~unrolled, ~state_before_right_hd, dn) => {
    let r_bound =
      switch (unrolled) {
      | [t, ..._] when Mtrl.is_space(LTerr.sort(t)) =>
        LSlope.height(unrolled) - 2
      | _ => LSlope.height(unrolled) - 1
      };
    let len = List.length(dn);
    // reverse to get top-down index which matches eqs
    List.rev(dn)
    |> List.mapi((i, t) => (i, t))
    // drop rolled terraces
    // |> Lists.take_while(~f=((i, _)) => i < dn_len - snd(rolled))
    |> List.fold_left_map(
         (state, (i, terr: LTerr.t)) => {
           if (i == len - 1) {
             state_before_right_hd := state;
           };

           let null = i >= r_bound && null;
           let eq = List.mem(i, eqs);

           let (p_l, cell) = LCell.depad(~side=L, terr.cell);
           let terr = {...terr, cell};

           let state = L.State.jump_cell(state, ~over=p_l);
           let (state, p) =
             T.Profile.mk_r(~sil=true, ~whole, ~state, ~eq, ~null, terr);

           (state, p);
         },
         state,
       )
    |> Tuples.map_snd(Layers.concat);
  };

  let mk =
      (
        ~whole: LCell.t,
        ~state: L.State.t,
        ~null as (null_l, null_r): (bool, bool),
        // indices into the slopes indicating which have delim-matching counterparts
        // in the surrounding context. indices assume slopes in top-down order. -1
        // is used to indicate that the top wald matches. matching info is needed
        // for proper layout traversal
        ~eqs as (eqs_l, eqs_r): (list(int), list(int)),
        ~rolled: ((int, Rel.t(_)), (int, Rel.t(_))),
        zigg: LZigg.t,
      ) => {
    let outer_sil = Silhouette.Outer.Profile.mk(~state, LZigg.flatten(zigg));

    let (r_l, r_r) = rolled;
    let (rolled_l, rolled_z, rolled_r) =
      LZigg.roll(~l=fst(r_l), ~r=fst(r_r), zigg);

    let rel_l =
      switch (snd(r_l)) {
      | Eq ()
      | Neq(Dir.R) => Either.Left()
      | Neq(L) =>
        // this state doesn't matter, just to satisfy types
        Right((state, LZigg.hd_block(~side=L, rolled_z)))
      };
    let (state, m_l) =
      mk_rolled(~side=L, ~whole, ~state, ~rel=rel_l, rolled_l);
    let (state, up) =
      mk_up(
        ~whole,
        ~state,
        ~null=null_l,
        ~eqs=eqs_l,
        ~unrolled=zigg.up,
        rolled_z.up,
      );

    let state_before_right_hd = ref(L.State.init);
    if (List.length(rolled_z.dn) == 0) {
      state_before_right_hd := state;
    };

    let (state, top) = {
      let null = (
        List.for_all(t => Mtrl.is_space(LTerr.sort(t)), zigg.up) && null_l,
        List.for_all(t => Mtrl.is_space(LTerr.sort(t)), zigg.dn) && null_r,
      );
      let eq = List.(mem(-1, eqs_l), mem(-1, eqs_r));
      W.Profile.mk(~sil=true, ~whole, ~state, ~null, ~eq, rolled_z.top);
    };

    let (state, dn) =
      mk_dn(
        ~whole,
        ~state,
        ~null=null_r,
        ~eqs=eqs_r,
        ~unrolled=zigg.dn,
        ~state_before_right_hd,
        rolled_z.dn,
      );
    let rel_r =
      switch (snd(r_r)) {
      | Eq ()
      | Neq(Dir.L) => Either.Left()
      | Neq(R) =>
        Right((state_before_right_hd^, LZigg.hd_block(~side=R, rolled_z)))
      };
    let (_state, m_r) =
      mk_rolled(~side=R, ~whole, ~state, ~rel=rel_r, rolled_r);
    {...Layers.concat([m_l, up, top, dn, m_r]), outer: [outer_sil]};
  };
};
