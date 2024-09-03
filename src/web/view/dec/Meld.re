module T = Token;
open Tylr_core;
module L = Layout;

module Profile = {
  type t = {
    indent: Loc.Col.t,
    range: (Loc.t, Loc.t),
    chain: Chain.t(Mtrl.Sorted.t, T.Profile.t),
  };

  let cells = p => Chain.loops(p.chain);
  let tokens = p => Chain.links(p.chain);

  let sort = (p: t) =>
    switch (tokens(p)) {
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
    let chain =
      Chain.combine(states, Meld.to_chain(m))
      |> Chain.map_loop(((_, c)) =>
           switch (Cell.get(c)) {
           | None => Mtrl.Space()
           | Some(M(_, w, _)) => Wald.sort(w)
           }
         )
      |> Chain.mapi_link((step, (state: L.State.t, tok)) => {
           let null = (
             step == 1 && null_l,
             step == Meld.length(m) - 2 && null_r,
           );
           T.Profile.mk(~loc=state.loc, ~null, tok);
         });
    {indent: state.ind, range: (s_l.loc, s_r.loc), chain};
  };
};

let sort_clss = (s: Mtrl.Sorted.t) =>
  switch (s) {
  | Space(_) => ["Space"]
  | Grout(s) => ["Grout", Sort.to_str(s)]
  | Tile(s) => ["Tile", Sort.to_str(s)]
  };

let mk_lines = (~font, p: Profile.t) =>
  switch (Profile.sort(p)) {
  | None => []
  | Some(_) =>
    let (sorts, toks) =
      Chain.mapi((i, c) => (i, c), (i, t) => (i, t), p.chain);
    let tok_rows =
      toks
      |> Base.List.group(~break=((_, l: T.Profile.t), (_, r: T.Profile.t)) =>
           l.loc.row != r.loc.row
         );
    let h_trunc = 0.3;
    let h_lines =
      tok_rows
      |> List.map(Stds.Lists.neighbors)
      |> List.concat_map(
           List.map((((step, l: T.Profile.t), (_, r: T.Profile.t))) =>
             Util.Svgs.Path.[
               m(~x=0, ~y=1) |> cmdfudge(~x=T.concave_adj +. h_trunc),
               h(~x=r.loc.col - (l.loc.col + l.len))
               |> cmdfudge(~x=-. T.concave_adj -. h_trunc),
             ]
             |> Util.Svgs.Path.view
             |> Util.Nodes.add_classes([
                  "child-line",
                  ...sort_clss(List.assoc(step + 1, sorts)),
                ])
             |> Stds.Lists.single
             |> Box.mk(~font, ~loc={...l.loc, col: l.loc.col + l.len})
           ),
         );
    let v_trunc = 0.15;
    let v_lines =
      Stds.Lists.neighbors(tok_rows)
      |> List.map(
           ((l: list((_, T.Profile.t)), r: list((_, T.Profile.t)))) => {
           assert(l != [] && r != []);
           let l_start = snd(List.hd(l)).loc;
           let r_start = snd(List.hd(r)).loc;
           // should this line extend to top or bottom (respectively) of row r?
           let v_delta =
             r_start.col == p.indent ? -. (1. +. 2. *. v_trunc) : -. v_trunc;
           // if the line extends to bottom, adjust to account for concave tip
           let h_delta = r_start.col == p.indent ? 0. : -. T.concave_adj;
           let dy = Float.of_int(r_start.row - l_start.row) +. v_delta;
           let path =
             dy < 0.5
               ? []
               : Util.Svgs.Path.[
                   m(~x=0, ~y=1) |> cmdfudge(~x=-. T.concave_adj, ~y=v_trunc),
                   V_({dy: dy}),
                   H_({dx: Float.of_int(r_start.col - p.indent) +. h_delta}),
                 ];
           path
           |> Util.Svgs.Path.view
           |> Util.Nodes.add_classes([
                "child-line",
                ...sort_clss(List.assoc(fst(List.hd(r)) - 1, sorts)),
              ])
           |> Stds.Lists.single
           |> Box.mk(~font, ~loc=l_start);
         });
    h_lines @ v_lines;
  };

let mk = (~font, p: Profile.t) =>
  List.map(T.mk(~font), Profile.tokens(p)) @ mk_lines(~font, p);
