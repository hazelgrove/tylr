open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Stds;

// to keep a reference to token dec
module T = Token;
open Tylr_core;

// just for convenience
module L = Layout;

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
