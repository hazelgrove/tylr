module T = Token;
open Tylr_core;
module L = Layout;

module Profile = {
  type t = {
    indent: Loc.Col.t,
    range: (Loc.t, Loc.t),
    tokens: list(T.Profile.t),
  };

  let mk =
      (
        ~state: L.State.t,
        M(t_l, _, _) as lyt: L.Tree.meld,
        M(l, _, r) as m: Meld.t,
      ) => {
    let (null_l, null_r) = Cell.Space.(is_space(l), is_space(r));
    let (s_end, states) = Layout.states(~init=state, lyt);
    let s_l =
      null_l ? L.State.jump_block(state, ~over=Tree.flatten(t_l)) : state;
    let s_r = null_r ? Chain.ft(states) : s_end;
    {
      indent: state.ind,
      range: (s_l.loc, s_r.loc),
      tokens:
        Chain.combine(states, Meld.to_chain(m))
        |> Chain.mapi_link((i, lk) => (i, lk))
        |> Chain.links
        |> List.map(((step, (state: L.State.t, tok))) => {
             let null = (
               step == 1 && null_l,
               step == Meld.length(m) - 2 && null_r,
             );
             T.Profile.mk(~loc=state.loc, ~null, tok);
           }),
    };
  };
  // let mk = (~state: Layout.State.t, m: Meld.t) => {
  //   let indent = {
  //     // todo: figure out better reorg for this recurring newline calc
  //     let M(l, _, _) = m;
  //     let newline = Dims.of_cell(l).height > 0;
  //     Layout.Ictx.middle(~newline, state.ctx);
  //   };
  //   let tokens =
  //     Meld.to_chain(m)
  //     |> Chain.mapi_link((step, tok) => (step, tok))
  //     |> Chain.unzip_links
  //     |> List.map(((pre, (step, tok), suf)) => {
  //          let l =
  //            Result.is_error(Chain.unlink(pre))
  //            && Cell.Space.is_space(Chain.hd(pre));
  //          let r =
  //            Result.is_error(Chain.unlink(suf))
  //            && Cell.Space.is_space(Chain.hd(suf));
  //          let pos =
  //            Cell.put(m)
  //            |> Cell.put_cursor(Point(Caret.focus([step, 0])))
  //            |> Layout.cursor(~state)
  //            |> Option.get
  //            |> Layout.Cursor.get_focus
  //            |> Option.get;
  //          T.Profile.mk(~pos, ~null=(l, r), tok);
  //        });
  //   let range = Layout.State.range(state, Dims.of_meld(m));
  //   {indent, tokens, range};
  // };
};

let mk = (~font, prof: Profile.t) => List.map(T.mk(~font), prof.tokens);
