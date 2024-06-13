module T = Token;
open Tylr_core;

module Profile = {
  type t = {
    indent: Layout.Col.t,
    tokens: list(T.Profile.t),
    range: (Layout.Pos.t, Layout.Pos.t),
  };

  let mk = (~state: Layout.State.t, m: Meld.t) => {
    let indent = {
      // todo: figure out better reorg for this recurring newline calc
      let M(l, _, _) = m;
      let newline = Dims.of_cell(l).height > 0;
      Layout.Ictx.middle(~newline, state.ctx);
    };
    let tokens =
      Meld.to_chain(m)
      |> Chain.mapi_link((step, tok) => (step, tok))
      |> Chain.unzip_links
      |> List.map(((pre, (step, tok), suf)) => {
           let l =
             Result.is_error(Chain.unlink(pre))
             && Cell.Space.is_space(Chain.hd(pre));
           let r =
             Result.is_error(Chain.unlink(suf))
             && Cell.Space.is_space(Chain.hd(suf));
           let pos =
             Cell.put(m)
             |> Cell.put_cursor(Point(Caret.focus([step, 0])))
             |> Layout.cursor(~state)
             |> Option.get
             |> Layout.Cursor.get_focus
             |> Option.get;
           T.Profile.mk(~pos, ~null=(l, r), tok);
         });
    let range = Layout.State.range(state, Dims.of_meld(m));
    {indent, tokens, range};
  };
};

let mk = (~font, prof: Profile.t) => List.map(T.mk(~font), prof.tokens);
