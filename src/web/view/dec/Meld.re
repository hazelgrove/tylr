module T = Token;
open Tylr_core;
module L = Layout;

module Profile = {
  type t = {
    indent: Loc.Col.t,
    range: (Loc.t, Loc.t),
    tokens: list(T.Profile.t),
  };

  let sort = (p: t) =>
    switch (p.tokens) {
    | [] => failwith("meld profile with no tokens")
    | [hd, ..._] => hd.style |> Option.map((style: T.Style.t) => style.sort)
    };

  let mk =
      (
        ~state: L.State.t,
        M(t_l, _, _) as lyt: L.Tree.meld,
        M(l, _, r) as m: Meld.t,
      ) => {
    let (null_l, null_r) = Cell.Space.(is_space(l), is_space(r));
    let (s_end, states) = Layout.states(~init=state, lyt);
    let s_l = null_l ? L.State.jump_cell(state, ~over=t_l) : state;
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
};

let mk_lines = (~font, prof: Profile.t) =>
  switch (Profile.sort(prof)) {
  | None => []
  | Some(s) =>
    let tok_rows =
      prof.tokens
      |> Base.List.group(~break=(l: T.Profile.t, r: T.Profile.t) =>
           l.loc.row != r.loc.row
         );
    let h_trunc = 0.15;
    let h_lines =
      tok_rows
      |> List.map(Stds.Lists.neighbors)
      |> List.concat_map(
           List.map(((l: T.Profile.t, r: T.Profile.t)) =>
             Util.Svgs.Path.[
               m(~x=0, ~y=1) |> cmdfudge(~x=T.concave_adj +. h_trunc),
               h(~x=r.loc.col - (l.loc.col + l.len))
               |> cmdfudge(~x=-. T.concave_adj -. h_trunc),
             ]
             |> Util.Svgs.Path.view
             |> Util.Nodes.add_classes(["child-line", Sort.to_str(s)])
             |> Stds.Lists.single
             |> Box.mk(~font, ~loc={...l.loc, col: l.loc.col + l.len})
           ),
         );
    let v_trunc = 0.05;
    let v_lines =
      Stds.Lists.neighbors(tok_rows)
      |> List.map(((l: list(T.Profile.t), r: list(T.Profile.t))) => {
           assert(l != [] && r != []);
           let l_start = List.hd(l).loc;
           let r_start = List.hd(r).loc;
           // should this line extend to top or bottom (respectively) of row r?
           let v_delta =
             r_start.col == prof.indent ? -. (1. +. 2. *. v_trunc) : 0.;
           // if the line extends to bottom, adjust to account for concave tip
           let h_delta = r_start.col == prof.indent ? 0. : -. T.concave_adj;
           Util.Svgs.Path.[
             m(~x=0, ~y=1) |> cmdfudge(~x=-. T.concave_adj, ~y=v_trunc),
             V_({dy: Float.of_int(r_start.row - l_start.row) +. v_delta}),
             H_({dx: Float.of_int(r_start.col - prof.indent) +. h_delta}),
           ]
           |> Util.Svgs.Path.view
           |> Util.Nodes.add_classes(["child-line", Sort.to_str(s)])
           |> Stds.Lists.single
           |> Box.mk(~font, ~loc=l_start);
         });
    h_lines @ v_lines;
  };

let mk = (~font, prof: Profile.t) =>
  List.map(T.mk(~font), prof.tokens) @ mk_lines(~font, prof);
