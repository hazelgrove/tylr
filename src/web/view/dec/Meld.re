open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Stds;

// to keep a reference to token dec
module T = Token;
open Tylr_core;

// just for convenience
module L = Layout;

let sort_clss = (s: Mtrl.Sorted.t) =>
  switch (s) {
  | Space(_) => ["Space"]
  | Grout(s) => ["Grout", Sort.to_str(s)]
  | Tile(s) => ["Tile", Sort.to_str(s)]
  };

module Child = {
  module Profile = {
    // [@deriving (show({with_path: false}), sexp, yojson)]
    // type row = {
    //   ind: Loc.Col.t,
    //   pad: (int, int),
    //   rest: int,
    // };
    // [@deriving (show({with_path: false}), sexp, yojson)]
    // type edge =
    //   | Delimited
    //   | Open(row);
    [@deriving (show({with_path: false}), sexp, yojson)]
    type row_metrics = (int, Block.Line.t);
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
      // indentation of delimiting tokens
      ind: Loc.Col.t,
      loc: Loc.t,
      dims: Dims.t,
      sort: Mtrl.Sorted.t,
      // whether or not the child lacks a delimiter on its left and right, in which case
      // decorations will depend on the metrics of the first/last rows of the child
      // no_delim: (option(row_metrics), option(row_metrics)),
      no_delim: (option(row_metrics), option(row_metrics)),
    };
  };

  let h_trunc = 0.2;
  let v_trunc = 0.15;

  let v_line_offset = 0.5;

  let includes_all_but_padding =
      (~side: Dir.t, col: Loc.Col.t, (ind, line): Profile.row_metrics) => {
    let (leading, rest) = Base.List.split_while(line, ~f=Token.Space.is);
    let (rest, _) = Base.List.split_while(rest, ~f=t => !Token.Space.is(t));
    switch (side) {
    | L =>
      let l = leading |> List.map(Token.length) |> List.fold_left((+), 0);
      col <= ind + l;
    | R =>
      let r =
        leading @ rest |> List.map(Token.length) |> List.fold_left((+), 0);
      col >= ind + r;
    };
  };

  let mk = (~font, p: Profile.t) => {
    let end_loc: Loc.t = Dims.skip(p.loc, ~over=p.dims, ~ind=p.ind);
    let Dims.{height, widths: (hd, _)} = p.dims;

    let l_closed = fst(p.no_delim) == None;
    let r_closed = snd(p.no_delim) == None;
    let r_closed_by_delim_after_newline = r_closed && end_loc.col == p.ind;

    let l_open_and_covers_row =
      fst(p.no_delim)
      |> Option.map(includes_all_but_padding(~side=L, p.loc.col))
      |> Option.value(~default=false);
    let r_open_and_covers_row =
      snd(p.no_delim)
      |> Option.map(includes_all_but_padding(~side=R, end_loc.col))
      |> Option.value(~default=false);

    let hd_line =
      hd.rest == 0 || height > 0 && l_open_and_covers_row
        ? []
        : Util.Svgs.Path.[
            m(~x=p.loc.col, ~y=p.loc.row + 1)
            |> cmdfudge(~x=l_closed ? T.concave_adj +. h_trunc : 0.)
            |> cmdfudge(~y=-. T.v_trunc -. T.stroke_shift),
            h(~x=p.loc.col + Dims.Width.total(hd))
            |> cmdfudge(
                 ~x=height == 0 && r_closed ? -. T.concave_adj -. h_trunc : 0.,
               ),
          ];
    let body_line =
      height <= 0 || r_closed_by_delim_after_newline && height <= 1
        ? []
        : Util.Svgs.Path.[
            m(~x=p.ind, ~y=p.loc.row)
            |> cmdfudge(~y=T.v_trunc +. T.stroke_shift)
            |> cmdfudge(~y=l_open_and_covers_row ? 0. : 1.)
            |> cmdfudge(~x=-. v_line_offset),
            v(~y=end_loc.row)
            |> cmdfudge(
                 ~y=
                   r_closed && end_loc.col == p.ind
                     ? 0. : 1. -. T.v_trunc -. T.stroke_shift,
               ),
          ];
    let ft_line =
      height == 0 || r_closed_by_delim_after_newline || r_open_and_covers_row
        ? []
        : Util.Svgs.Path.[
            h(~x=end_loc.col)
            |> cmdfudge(~x=r_closed ? -. T.concave_adj -. h_trunc : 0.),
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
  exception No_tokens;

  let cells = p => Chain.loops(p);
  let tokens = p => Chain.links(p);

  let sort = (p: t) =>
    switch (tokens(p)) {
    | [] => raise(No_tokens)
    | [hd, ..._] => hd.style |> Option.map((style: T.Style.t) => style.sort)
    };

  let mk = (~tree: LCell.t, ~path: Path.t, m: Meld.t) => {
    let M(l, _, r) = m;
    let (null_l, null_r) = Cell.Space.(is_space(l), is_space(r));
    let n = Meld.length(m);

    let invalid = Invalid_argument("Meld.Profile.mk");
    let (state, t) = L.state_of_path(~tree, path);

    let M(t_l, w, t_r) =
      Options.get_exn(invalid, t).meld |> Options.get_exn(invalid);
    let (p_l, t_l) = LCell.depad(~side=L, t_l);
    let (_, t_r) = LCell.depad(~side=R, t_r);
    let lyt = Meld.Base.mk(~l=t_l, w, ~r=t_r);

    let s_init = state |> L.State.jump_cell(~over=p_l);
    let s_tok = L.State.jump_cell(s_init, ~over=t_l);
    let (s_end, states) = Layout.states(~init=s_init, lyt);

    let l_line = L.nth_line(tree, s_init.loc.row);
    let r_line = L.nth_line(tree, s_end.loc.row);

    Chain.combine(Meld.Base.to_chain(lyt), Meld.to_chain(m))
    |> Chain.combine(states)
    |> Chain.mapi_loop((step, (s: L.State.t, (t_cell, cell))) => {
         let no_delim = (
           step > 0 ? None : Some(l_line),
           step < n - 1 ? None : Some(r_line),
         );
         let sort =
           switch (Cell.get(cell)) {
           | None => Mtrl.Space()
           | Some(M(_, w, _)) => Wald.sort(w)
           };
         let dims = Dims.of_block(LCell.flatten(t_cell));
         let loc = s.loc;
         let ind = s_tok.ind;
         Child.Profile.{ind, loc, dims, sort, no_delim};
       })
    |> Chain.mapi_link((step, (state: L.State.t, (_, tok))) => {
         let null = (step == 1 && null_l, step == n - 2 && null_r);
         T.Profile.mk(~loc=state.loc, ~null, tok);
       });
  };
};

let mk = (~font, p: Profile.t) =>
  List.map(T.mk(~font), Profile.tokens(p))
  @ List.map(Child.mk(~font), Profile.cells(p));
