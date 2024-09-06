open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

module T = Token;
open Tylr_core;
module L = Layout;

let sort_clss = (s: Mtrl.Sorted.t) =>
  switch (s) {
  | Space(_) => ["Space"]
  | Grout(s) => ["Grout", Sort.to_str(s)]
  | Tile(s) => ["Tile", Sort.to_str(s)]
  };

module Child = {
  module Profile = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
      // indentation of delimiting tokens
      ind: Loc.Col.t,
      loc: Loc.t,
      dims: Dims.t,
      sort: Mtrl.Sorted.t,
      delim: (bool, bool),
    };
  };

  let h_trunc = 0.3;
  let v_trunc = 0.15;

  let v_line_offset = 0.5;

  let mk = (~font, p: Profile.t) => {
    let end_loc: Loc.t = Dims.skip(p.loc, ~over=p.dims, ~ind=p.ind);
    let Dims.{height, widths: (hd, _)} = p.dims;
    let hd_line =
      hd.rest == 0
        ? []
        : Util.Svgs.Path.[
            m(~x=p.loc.col, ~y=p.loc.row + 1)
            |> cmdfudge(~x=fst(p.delim) ? T.concave_adj +. h_trunc : 0.),
            h(~x=p.loc.col + Dims.Width.total(hd))
            |> cmdfudge(
                 ~x=
                   height == 0 && snd(p.delim)
                     ? -. T.concave_adj -. h_trunc : 0.,
               ),
          ];
    let body_line =
      height <= 0 || snd(p.delim) && height <= 0
        ? []
        : Util.Svgs.Path.[
            m(~x=p.ind, ~y=p.loc.row + 1)
            |> cmdfudge(~x=-. v_line_offset, ~y=v_trunc),
            v(~y=end_loc.row)
            |> cmdfudge(
                 ~y=snd(p.delim) && end_loc.col == p.ind ? -. v_trunc : 1.,
               ),
          ];
    let ft_line =
      height == 0 || end_loc.col == p.ind
        ? []
        : Util.Svgs.Path.[
            h(~x=end_loc.col)
            |> cmdfudge(~x=snd(p.delim) ? -. T.concave_adj -. h_trunc : 0.),
          ];

    hd_line
    @ body_line
    @ ft_line
    |> Util.Svgs.Path.view
    |> Util.Nodes.add_classes(["child-line", ...sort_clss(p.sort)])
    |> Stds.Lists.single
    |> Box.mk(~font, ~loc={row: 0, col: 0});
  };
};

module Profile = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Chain.t(Child.Profile.t, T.Profile.t);
  // indentation of the meld's tokens
  // indent: Loc.Col.t,
  // // endpoints of the full meld range sans padding
  // range: (Loc.t, Loc.t),
  // chain: Chain.t(Child.Profile.t, T.Profile.t),
  // whether or not the range ends start/end their respective rows.
  // left type is hack where Some(col) corresponds to false and passes
  // the max col of the row containing the left end for decoration purposes.
  // full_row: (option(Loc.Col.t), bool),
  // chain: Chain.t(Mtrl.Sorted.t, T.Profile.t),
  // };
  // at least one token expected in chain
  exception No_tokens;

  let cells = p => Chain.loops(p);
  let tokens = p => Chain.links(p);

  let sort = (p: t) =>
    switch (tokens(p)) {
    | [] => raise(No_tokens)
    | [hd, ..._] => hd.style |> Option.map((style: T.Style.t) => style.sort)
    };

  let mk =
      (
        ~state: L.State.t,
        M(t_l, _, _) as lyt: L.Tree.meld,
        M(l, _, r) as m: Meld.t,
      ) => {
    let (null_l, null_r) = Cell.Space.(is_space(l), is_space(r));
    let (_, states) = Layout.states(~init=state, lyt);
    let s_tok = L.State.jump_cell(state, ~over=t_l);
    let n = Meld.length(m);
    Chain.combine(L.Tree.to_chain(lyt), Meld.to_chain(m))
    |> Chain.combine(states)
    |> Chain.mapi_loop((step, (s: L.State.t, (t_cell, cell))) => {
         let delim = (step > 0, step < n - 1);
         let sort =
           switch (Cell.get(cell)) {
           | None => Mtrl.Space()
           | Some(M(_, w, _)) => Wald.sort(w)
           };
         let dims = Dims.of_block(Tree.flatten(t_cell));
         let loc = s.loc;
         let ind = s_tok.ind;
         let r = Child.Profile.{ind, loc, dims, sort, delim};
         Stds.P.show("r", Child.Profile.show(r));
         r;
       })
    |> Chain.mapi_link((step, (state: L.State.t, (_, tok))) => {
         let null = (step == 1 && null_l, step == n - 2 && null_r);
         T.Profile.mk(~loc=state.loc, ~null, tok);
       });
  };
  // weird abstraction boundary here with range and full_row getting passed in
  // let mk =
  //     (
  //       ~range,
  //       ~full_row,
  //       ~state: L.State.t,
  //       M(t_l, _, _) as lyt: L.Tree.meld,
  //       M(l, _, r) as m: Meld.t,
  //     ) => {
  //   let (null_l, null_r) = Cell.Space.(is_space(l), is_space(r));
  //   let (_, states) = Layout.states(~init=state, lyt);
  //   let s_tok = L.State.jump_cell(state, ~over=t_l);
  //   let chain =
  //     Chain.combine(states, Meld.to_chain(m))
  //     |> Chain.map_loop(((_, c)) =>
  //          switch (Cell.get(c)) {
  //          | None => Mtrl.Space()
  //          | Some(M(_, w, _)) => Wald.sort(w)
  //          }
  //        )
  //     |> Chain.mapi_link((step, (state: L.State.t, tok)) => {
  //          let null = (
  //            step == 1 && null_l,
  //            step == Meld.length(m) - 2 && null_r,
  //          );
  //          T.Profile.mk(~loc=state.loc, ~null, tok);
  //        });
  //   {indent: s_tok.ind, range, full_row, chain};
  // };
};

// [@warning "-27"]
// let uni_lines = (~font, p: Profile.t) =>
//   switch (Profile.sort(p)) {
//   | None => []
//   | Some(_) =>
//     open Util.Svgs.Path;
//     let (end_l, end_r) = p.range;
//     let (fr_l, fr_r) = p.full_row;
//     let line_l = {
//       let (s_l, t_l, _) =
//         Chain.unlink(p.chain) |> Stds.Result.get_exn(Profile.No_tokens);
//       if (end_l.row == t_l.loc.row) {
//         [
//           m(~x=t_l.loc.col, ~y=t_l.loc.row + 1)
//           |> cmdfudge(~x=-. T.concave_adj -. Child.h_trunc),
//           h(~x=end_l.col),
//         ]
//         // |> cmdfudge(~x=-. T.concave_adj -. h_trunc),
//         |> Util.Svgs.Path.view
//         |> Util.Nodes.add_classes(["child-line", ...sort_clss(s_l)])
//         |> Stds.Lists.single
//         |> Box.mk(~font, ~loc=Loc.zero);
//       } else {
//         open Stds;
//         P.log("drawing l arm");
//         P.show("p", Profile.show(p));
//         // m_first.origin.col == indent
//         //   ? [
//         //     m(~x=m_last_of_first.last.col - m_first.origin.col, ~y=0),
//         //     // TODO(d) need to take max of all rows, not just top
//         //     h(~x=max_col - m_first.origin.col),
//         //     shadowfudge(v(~y=l.row - m_last_of_first.origin.row)),
//         //   ]
//         //   : [
//         //     shadowfudge(m(~x=0, ~y=1)),
//         //     h(~x=indent - m_first.origin.col),
//         //     shadowfudge(v(~y=l.row + 1 - m_first.origin.row)),
//         //     h(~x=max_col - m_first.origin.col),
//         //     shadowfudge(v(~y=l.row - m_first.origin.row)),
//         //   ]
//         (
//           t_l.loc.col == p.indent
//             ? [m(~x=t_l.loc.col, ~y=t_l.loc.row)]
//             : [
//               m(~x=t_l.loc.col, ~y=t_l.loc.row + 1)
//               |> cmdfudge(~x=-. T.concave_adj -. h_trunc),
//               h(~x=p.indent),
//             ]
//         )
//         @ (
//           switch (fr_l) {
//           | None => [v(~y=end_l.row)]
//           | Some(col) => [
//               v(~y=end_l.row + 1),
//               m(~x=end_l.col, ~y=end_l.row + 1),
//               h(~x=col),
//             ]
//           }
//         )
//         // |> cmdfudge(~x=-. T.concave_adj -. h_trunc),
//         |> Util.Svgs.Path.view
//         |> Util.Nodes.add_classes(["child-line", ...sort_clss(s_l)])
//         |> Stds.Lists.single
//         |> Box.mk(~font, ~loc=Loc.zero);
//       };
//     };
//     [line_l];
//   // let line_r = {
//   //   let (s_r, t_r, _) =
//   //     Chain.unlink(Chain.rev(p.chain))
//   //     |> Stds.Result.get_exn(Profile.No_tokens);
//   //   let start =
//   //     m(~x=t_r.loc.col + t_r.len, ~y=t_r.loc.row + 1)
//   //     |> cmdfudge(~x=T.concave_adj +. h_trunc);
//   //   if (end_r.row == t_r.loc.row) {
//   //     [start, h(~x=end_r.col)]
//   //     // |> cmdfudge(~x=-. T.concave_adj -. h_trunc),
//   //     |> Util.Svgs.Path.view
//   //     |> Util.Nodes.add_classes(["child-line", ...sort_clss(s_r)])
//   //     |> Stds.Lists.single
//   //     |> Box.mk(~font, ~loc=Loc.zero);
//   //   } else {
//   //     [
//   //       start,
//   //       h(~x=p.indent),
//   //       ...switch (fr_l) {
//   //          | None => [v(~y=end_l.row)]
//   //          | Some(col) => [
//   //              v(~y=end_l.row + 1),
//   //              m(~x=end_l.col, ~y=end_l.row + 1),
//   //              h(~x=col),
//   //            ]
//   //          },
//   //     ]
//   //     // |> cmdfudge(~x=-. T.concave_adj -. h_trunc),
//   //     |> Util.Svgs.Path.view
//   //     |> Util.Nodes.add_classes(["child-line", ...sort_clss(s_l)])
//   //     |> Stds.Lists.single
//   //     |> Box.mk(~font, ~loc=Loc.zero);
//   //   };
//   // };
//   // [line_l, line_r];
//   };

// let mk_lines = (~font, p: Profile.t) =>
//   switch (Profile.sort(p)) {
//   | None => []
//   | Some(_) =>
//     // index elements for index-based lookup of sorts adjacent to tokens
//     let (sorts, toks) =
//       Chain.mapi((i, c) => (i, c), (i, t) => (i, t), p.chain);
//     // group tokens by row
//     let tok_rows =
//       toks
//       |> Base.List.group(~break=((_, l: T.Profile.t), (_, r: T.Profile.t)) =>
//            l.loc.row != r.loc.row
//          );
//     // bi lines within each row
//     let h_lines =
//       tok_rows
//       |> List.map(Stds.Lists.neighbors)
//       |> List.concat_map(
//            List.map((((step, l: T.Profile.t), (_, r: T.Profile.t))) =>
//              Util.Svgs.Path.[
//                m(~x=0, ~y=1) |> cmdfudge(~x=T.concave_adj +. Child.h_trunc),
//                h(~x=r.loc.col - (l.loc.col + l.len))
//                |> cmdfudge(~x=-. T.concave_adj -. Child.h_trunc),
//              ]
//              |> Util.Svgs.Path.view
//              |> Util.Nodes.add_classes([
//                   "child-line",
//                   ...sort_clss(List.assoc(step + 1, sorts)),
//                 ])
//              |> Stds.Lists.single
//              |> Box.mk(~font, ~loc={...l.loc, col: l.loc.col + l.len})
//            ),
//          );
//     // bi lines between rows
//     let v_lines =
//       Stds.Lists.neighbors(tok_rows)
//       |> List.map(
//            ((l: list((_, T.Profile.t)), r: list((_, T.Profile.t)))) => {
//            assert(l != [] && r != []);
//            let l_start = snd(List.hd(l)).loc;
//            let r_start = snd(List.hd(r)).loc;
//            // should this line extend to top or bottom (respectively) of row r?
//            let v_delta =
//              r_start.col == p.indent ? -. (1. +. 2. *. Child.v_trunc) : -. Child.v_trunc;
//            // if the line extends to bottom, adjust to account for concave tip
//            let h_delta = r_start.col == p.indent ? 0. : -. T.concave_adj;
//            let dy = Float.of_int(r_start.row - l_start.row) +. v_delta;
//            let path =
//              dy < 0.5
//                ? []
//                : Util.Svgs.Path.[
//                    m(~x=0, ~y=1) |> cmdfudge(~x=-. T.concave_adj, ~y=Child.v_trunc),
//                    V_({dy: dy}),
//                    H_({dx: Float.of_int(r_star  t.col - p.indent) +. h_delta}),
//                  ];
//            path
//            |> Util.Svgs.Path.view
//            |> Util.Nodes.add_classes([
//                 "child-line",
//                 ...sort_clss(List.assoc(fst(List.hd(r)) - 1, sorts)),
//               ])
//            |> Stds.Lists.single
//            |> Box.mk(~font, ~loc=l_start);
//          });
//     h_lines @ v_lines;
//   };

let mk = (~font, p: Profile.t) =>
  List.map(T.mk(~font), Profile.tokens(p))
  @ List.map(Child.mk(~font), Profile.cells(p));
