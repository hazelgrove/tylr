open Stds;

module Tok = Token;
module T = Terr;
module W = Wald;
module M = Meld;
open Tylr_core;

module L = Layout;

module Profile = {
  type t = Layers.t;

  let mk_rolled_l = (~whole, ~state as s_init, ~merge=?, rolled: LCell.t) => {
    switch (rolled.meld) {
    | None => ((s_init, []), Layers.empty)
    | Some(m) =>
      switch (merge) {
      | None => M.Profile.mk(~sil=true, ~whole, ~state=s_init, m)
      | Some(hd_terr) =>
        let s_tok = L.State.jump_cell(s_init, ~over=rolled);
        let ((s_post_cell, rolled_ns), rolled) =
          Child.Profile.mk(
            ~sil=true,
            ~whole,
            ~ind=L.Indent.curr(s_tok.ind),
            ~state=s_init,
            ~null=(true, false),
            rolled,
          );
        L.State.clear_log();
        let s_post_hd = L.State.jump_terr_l(s_post_cell, ~over=hd_terr);
        let hd_ns = L.State.get_log();
        let sil =
          Silhouette.Inner.Profile.mk(
            ~is_space=Mtrl.is_space(LMeld.sort(m)),
            Lists.consnoc_pairs(s_init, rolled_ns @ hd_ns, s_post_hd),
          );
        let p = Layers.mk(~inner=[sil], ~cells=[rolled], ());
        ((s_post_cell, rolled_ns), p);
      }
    };
  };

  let mk_rolled_r = (~whole, ~state, ~merge=?, rolled: LCell.t) => {
    switch (rolled.meld) {
    | None => ((state, []), Layers.empty)
    | Some(m) =>
      switch (merge) {
      | None => M.Profile.mk(~sil=true, ~whole, ~state, m)
      | Some((s_before_hd, hd_terr)) =>
        L.State.clear_log();
        let s_post_hd = L.State.jump_terr(s_before_hd, ~over=hd_terr);
        let hd_ns = L.State.get_log();
        let ((s_post_rolled, rolled_ns), rolled) =
          Child.Profile.mk(
            ~sil=true,
            ~whole,
            ~ind=L.Indent.curr(s_post_hd.ind),
            ~state,
            ~null=(false, true),
            rolled,
          );
        let sil =
          Silhouette.Inner.Profile.mk(
            ~is_space=Mtrl.is_space(LMeld.sort(m)),
            Lists.consnoc_pairs(
              s_before_hd,
              hd_ns @ rolled_ns,
              s_post_rolled,
            ),
          );
        let p = Layers.mk(~inner=[sil], ~cells=[rolled], ());
        ((s_post_rolled, rolled_ns), p);
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
    let (state, nss_terrs) =
      // reverse to get top-down index which matches eqs
      List.rev(up)
      |> List.mapi((i, t) => (i, t))
      // drop rolled terraces
      // |> Lists.take_while(~f=((i, _)) => i < up_len - fst(rolled))
      // reverse back to go left to right
      |> List.rev
      |> Lists.fold_map(
           ~init=state,
           ~f=(state, (i, terr: LTerr.t)) => {
             let null = i >= l_bound && null;
             let eq = List.mem(i, eqs);
             let ((state, terr_ns), p) =
               T.Profile.mk_l(~sil=true, ~whole, ~state, ~eq, ~null, terr);
             (state, (terr_ns, p));
           },
         );
    let (nss, terrs) = List.split(nss_terrs);
    let ns = List.concat(nss);
    let p = Layers.concat(terrs);
    ((state, ns), p);
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
    let (state, nss_terrs) =
      // reverse to get top-down index which matches eqs
      List.rev(dn)
      |> List.mapi((i, t) => (i, t))
      // drop rolled terraces
      // |> Lists.take_while(~f=((i, _)) => i < dn_len - snd(rolled))
      |> Lists.fold_map(
           ~init=state,
           ~f=(state, (i, terr: LTerr.t)) => {
             if (i == len - 1) {
               state_before_right_hd := state;
             };
             let null = i >= r_bound && null;
             let eq = List.mem(i, eqs);
             let ((state, terr_ns), p) =
               T.Profile.mk_r(~sil=true, ~whole, ~state, ~eq, ~null, terr);
             (state, (terr_ns, p));
           },
         );
    let (nss, terrs) = List.split(nss_terrs);
    let ns = List.concat(nss);
    let p = Layers.concat(terrs);
    ((state, ns), p);
  };

  let mk =
      (
        ~whole: LCell.t,
        ~state as s_init: L.State.t,
        ~null as (null_l, null_r): (bool, bool),
        // indices into the slopes indicating which have delim-matching counterparts
        // in the surrounding context. indices assume slopes in top-down order. -1
        // is used to indicate that the top wald matches. matching info is needed
        // for proper layout traversal
        ~eqs as (eqs_l, eqs_r): (list(int), list(int)),
        ~rolled: ((int, Rel.t(_)), (int, Rel.t(_))),
        zigg: LZigg.t,
      ) => {
    let (r_l, r_r) = rolled;
    let (rolled_l, rolled_z, rolled_r) =
      LZigg.roll(~l=fst(r_l), ~r=fst(r_r), zigg);

    let merge_l =
      switch (snd(r_l)) {
      | Eq ()
      | Neq(Dir.R) => None
      | Neq(L) =>
        let hd =
          switch (rolled_z.up) {
          | [] => Terr.Base.{wald: rolled_z.top, cell: LCell.empty}
          | [hd, ..._] => hd
          };
        Some(hd);
      };
    let ((state, m_l_ns), m_l) =
      mk_rolled_l(~whole, ~state=s_init, ~merge=?merge_l, rolled_l);
    let ((state, up_ns), up) =
      mk_up(
        ~whole,
        ~state,
        ~null=null_l,
        ~eqs=eqs_l,
        ~unrolled=zigg.up,
        rolled_z.up,
      );

    let state_before_right_hd = ref(state);

    let ((state, top_ns), top) = {
      let null = (
        List.for_all(t => Mtrl.is_space(LTerr.sort(t)), zigg.up) && null_l,
        List.for_all(t => Mtrl.is_space(LTerr.sort(t)), zigg.dn) && null_r,
      );
      let eq = List.(mem(-1, eqs_l), mem(-1, eqs_r));
      W.Profile.mk(~sil=true, ~whole, ~state, ~null, ~eq, rolled_z.top);
    };

    let ((state, dn_ns), dn) =
      mk_dn(
        ~whole,
        ~state,
        ~null=null_r,
        ~eqs=eqs_r,
        ~unrolled=zigg.dn,
        // let dn traversal update this if there are any terraces
        ~state_before_right_hd,
        rolled_z.dn,
      );
    let merge_r =
      switch (snd(r_r)) {
      | Eq ()
      | Neq(Dir.L) => None
      | Neq(R) =>
        let hd =
          switch (rolled_z.dn) {
          | [] => Terr.Base.{wald: rolled_z.top, cell: LCell.empty}
          | [hd, ..._] => hd
          };
        Some((state_before_right_hd^, hd));
      };
    let ((state, m_r_ns), m_r) =
      mk_rolled_r(~whole, ~state, ~merge=?merge_r, rolled_r);

    let ns = List.concat([m_l_ns, up_ns, top_ns, dn_ns, m_r_ns]);
    let outer_sil =
      Silhouette.Outer.Profile.mk(Lists.consnoc_pairs(s_init, ns, state));

    {...Layers.concat([m_l, up, top, dn, m_r]), outer: [outer_sil]};
  };
};
