open Tylr_core;
module L = Layout;
module Style = {
  type t =
    | Inner
    | Outer;
  let to_str =
    fun
    | Inner => "inner"
    | Outer => "outer";
};
module Profile = {
  type t = {
    first_row: Loc.Row.t,
    lines: list((Loc.Col.t, Loc.Col.t)),
    style: Style.t,
  };
  let mk = (~style: Style.t, ~state: L.State.t, block: Block.t) => {
    let first_row = state.loc.row;
    let lines =
      Block.flatten(block)
      |> Chain.fold_left_map(
           line => {
             let len = Block.Line.len(line);
             let s = L.State.map(Loc.shift(len), state);
             (s, (state.loc.col, state.loc.col + len));
           },
           (state, ind, line) => {
             let s = L.State.return(state, ind);
             let len = Block.Line.len(line);
             let s' = L.State.map(Loc.shift(len), s);
             (s', (), (s.loc.col, state.loc.col + len));
           },
         )
      |> snd
      |> fst;
    {first_row, lines, style};
  };
};
let mk = (~font, p: Profile.t) => {
  let (l, r) = List.split(p.lines);
  let l_path =
    Chain.nlist(List.rev(l))
    |> Chain.map_linked((_, (), above) => Util.Svgs.Path.h(~x=above))
    |> Chain.map_loop(_ => Util.Svgs.Path.v_(~dy=-1))
    |> Chain.to_list(Fun.id, Fun.id);
  let r_path =
    Chain.nlist(r)
    |> Chain.map_linked((_, (), below) => Util.Svgs.Path.h(~x=below))
    |> Chain.map_loop(_ => Util.Svgs.Path.v_(~dy=1))
    |> Chain.to_list(Fun.id, Fun.id);
  let path =
    List.concat(
      Util.Svgs.Path.[
        [m(~x=List.hd(l), ~y=p.first_row), h(~x=List.hd(r))],
        r_path,
        [h(~x=Stds.Lists.ft_exn(l))],
        l_path,
        [Z],
      ],
    )
    |> Util.Svgs.Path.view
    |> Util.Nodes.add_classes(["silhouette", Style.to_str(p.style)]);
  Box.mk(~font, ~loc=Loc.zero, [path]);
};
