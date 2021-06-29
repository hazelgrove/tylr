open Sexplib.Std;
open Util;
open Cor;
open OptUtil.Syntax;

/**
 * A data structure that accompanies a layout
 * (see `Layout.t`) and contains relative paths to
 * decorations of the layout
 */
[@deriving sexp]
type t = {
  // TODO rename to anchors
  caret: option(Path.range),
  siblings: option((Path.steps, list(Path.caret_step))),
  root_term: option((Path.steps, Skel.t)),
  filtered_selems: option((Path.steps, list(Path.selem_step))),
  logo_selems: list(Path.selem_step),
};

let mk =
    (
      ~caret=?,
      ~siblings=?,
      ~root_term=?,
      ~filtered_selems=?,
      ~logo_selems=[],
      (),
    ) => {
  caret,
  siblings,
  root_term,
  filtered_selems,
  logo_selems,
};
let empty = mk();

let root_term = ((subject, frame): Zipper.t) => {
  let zip_up =
      (subject: Selection.t, frame: Frame.t)
      : option((Tile.t, ListFrame.t(Tile.t), Frame.t)) => {
    let tiles = Option.get(Selection.get_tiles(subject));
    let get_pat = () => Option.get(Tiles.get_pat(tiles));
    let get_exp = () => Option.get(Tiles.get_exp(tiles));
    switch (frame) {
    | Pat(Paren_body((tframe, frame))) =>
      let tile = Tile.Pat(Paren(get_pat()));
      let tframe = TupleUtil.map2(Tiles.of_pat, tframe);
      Some((tile, tframe, Pat(frame)));
    | Pat(Lam_pat((tframe, frame))) =>
      let tile = Tile.Exp(Lam(get_pat()));
      let tframe = TupleUtil.map2(Tiles.of_exp, tframe);
      Some((tile, tframe, Exp(frame)));
    | Pat(Let_pat(def, (tframe, frame))) =>
      let tile = Tile.Exp(Let(get_pat(), def));
      let tframe = TupleUtil.map2(Tiles.of_exp, tframe);
      Some((tile, tframe, Exp(frame)));
    | Exp(Paren_body((tframe, frame))) =>
      let tile = Tile.Exp(Paren(get_exp()));
      let tframe = TupleUtil.map2(Tiles.of_exp, tframe);
      Some((tile, tframe, Exp(frame)));
    | Exp(Let_def(p, (tframe, frame))) =>
      let tile = Tile.Exp(Let(p, get_exp()));
      let tframe = TupleUtil.map2(Tiles.of_exp, tframe);
      Some((tile, tframe, Exp(frame)));
    | Exp(Root) => None
    };
  };
  switch (subject) {
  | Selecting(_)
  | Restructuring(_) => None
  | Pointing(sframe) =>
    let tframe =
      sframe
      |> TupleUtil.map2(Selection.get_tiles)
      |> TupleUtil.map2(
           OptUtil.get_or_fail("expected prefix/suffix to consist of tiles"),
         );
    let skel_at = (n, tiles) => Skel.skel_at(n, Parser.associate(tiles));
    let (steps, skel) =
      switch (tframe) {
      | (prefix, []) =>
        switch (zip_up(Selection.of_tiles(List.rev(prefix)), frame)) {
        | None =>
          let tiles = List.rev(prefix);
          (Path.mk_steps(frame), skel_at(List.length(tiles) - 1, tiles));
        | Some((root_tile, (prefix, _) as tframe, outer_frame)) =>
          let tiles = ListFrame.to_list(~subject=[root_tile], tframe);
          (
            Path.mk_steps(outer_frame),
            skel_at(List.length(prefix), tiles),
          );
        }
      | (prefix, [_, ..._]) =>
        let tiles = ListFrame.to_list(tframe);
        (Path.mk_steps(frame), skel_at(List.length(prefix), tiles));
      };
    Some((steps, skel));
  };
};

let siblings = (subject: Subject.t) =>
  switch (subject) {
  | Pointing(sframe) =>
    ListUtil.range(List.length(ListFrame.to_list(sframe)) + 1)
  | Selecting(selection, (prefix, suffix)) =>
    let len_pre = List.length(prefix);
    let len_sel = List.length(selection);
    let len_suf = List.length(suffix);
    ListUtil.range(len_pre + 1)
    @ ListUtil.range(~lo=len_pre + len_sel, len_pre + len_sel + len_suf + 1);
  | Restructuring(selection, (prefix, suffix) as rframe) =>
    if (Selection.is_whole_any(selection)) {
      ListUtil.range(List.length(ListFrame.to_list(rframe)) + 1);
    } else {
      let (tiles_pre, prefix) = ListUtil.take_while(Selem.is_tile, prefix);
      let (tiles_suf, _) = ListUtil.take_while(Selem.is_tile, suffix);
      ListUtil.range(
        ~lo=List.length(prefix),
        List.(length(prefix) + length(tiles_pre) + length(tiles_suf) + 1),
      );
    }
  };

let filtered_selems = (~frame_sort, subject: Subject.t) => {
  switch (subject) {
  | Pointing(_) => None
  | Selecting(selection, (prefix, _) as sframe) =>
    let len_pre = List.length(prefix);
    let len_sel = List.length(selection);
    let selems =
      ListFrame.to_list(~subject=selection, sframe)
      |> List.mapi((i, selem) => (i, selem))
      |> List.filter_map(((i, selem)) =>
           Selem.sort(selem) == frame_sort
           && Selem.is_tile(selem)
           || len_pre <= i
           && i <= len_pre
           + len_sel
             ? None : Some(i)
         );
    Some(selems);
  | Restructuring(_, sframe) =>
    let selems =
      ListFrame.to_list(sframe)
      |> List.mapi((i, selem) => (i, selem))
      |> List.filter_map(((i, selem)) =>
           Selem.sort(selem) == frame_sort && Selem.is_tile(selem)
             ? None : Some(i)
         );
    Some(selems);
  };
};

let of_zipper = ((subject, frame) as zipper: Zipper.t) => {
  let (steps, _) as caret_range = Path.mk_range(zipper);
  let filtered_selems = {
    let+ selems = filtered_selems(~frame_sort=Frame.sort(frame), subject);
    (steps, selems);
  };
  mk(
    ~caret=caret_range,
    ~siblings=(steps, siblings(subject)),
    ~root_term=?root_term(zipper),
    ~filtered_selems?,
    (),
  );
};

let take_two_step = (two_step: Path.two_step, paths: t): t => {
  // print_endline("DecPaths.take_two_step");
  // print_endline(Sexplib.Sexp.to_string(Path.sexp_of_two_step(two_step)));
  // print_endline(Sexplib.Sexp.to_string_hum(sexp_of_t(paths)));
  let {caret, siblings, root_term, filtered_selems, logo_selems: _} = paths;
  let step_or_prune_steps =
    fun
    | [two_step', ...steps] when two_step' == two_step => Some(steps)
    | _ => None;
  let step_or_prune:
    'a.
    option((Path.steps, 'a)) => option((Path.steps, 'a))
   =
    p => {
      let* (steps, a) = p;
      let+ steps = step_or_prune_steps(steps);
      (steps, a);
    };
  let paths = {
    caret: step_or_prune(caret),
    siblings: step_or_prune(siblings),
    root_term: step_or_prune(root_term),
    filtered_selems: step_or_prune(filtered_selems),
    logo_selems: [],
  };
  //print_endline(Sexplib.Sexp.to_string_hum(sexp_of_t(paths)));
  paths;
};

// TODO have everything be computed from here
let current_bidelimited =
    (~origin: int, ~sort: Sort.t, layout: Layout.t, paths: t) => {
  let {root_term, logo_selems, caret: _, siblings: _, filtered_selems: _} = paths;
  let root_term =
    switch (root_term) {
    | Some(([], skel)) =>
      let root_tile_profile = {
        let root_step = Skel.root_index(skel);
        let (measurement, color, shape, selem_l) =
          Layout.find_selem(~origin, root_step, layout);
        let empty_holes = Layout.selem_holes(selem_l);
        let (open_children, closed_children) =
          Layout.selem_children(selem_l);
        Dec.Profile.Selem({
          measurement,
          open_children,
          closed_children,
          empty_holes,
          color,
          style: Root,
          shape,
        });
      };
      let uni_child_profiles =
        Skel.children(skel)
        |> List.map(((side, skel)) => {
             let range = Skel.range(skel);
             let measurement = Layout.measure_range(~origin, range, layout);
             Dec.Profile.UniChild({measurement, sort, side});
           });
      [root_tile_profile, ...uni_child_profiles];
    | _ => []
    };
  let logo_selems =
    logo_selems
    |> List.map(step => {
         let (measurement, color, shape, _) =
           Layout.find_selem(~origin, step, layout);
         Dec.Profile.Selem({
           measurement,
           open_children: [],
           closed_children: [],
           empty_holes: [],
           color,
           style: Logo,
           shape,
         });
       });
  List.concat([root_term, logo_selems]);
};

let current_selem =
    (
      ~measurement: Layout.measurement,
      step: Path.selem_step,
      color: Color.t,
      shape: Layout.selem_shape,
      selem_l: Layout.t,
      paths: t,
    ) => {
  let {filtered_selems, caret: _, siblings: _, root_term: _, logo_selems: _} = paths;
  switch (filtered_selems) {
  | Some(([], filtered_selems)) when List.mem(step, filtered_selems) =>
    let empty_holes = Layout.selem_holes(selem_l);
    let (open_children, closed_children) = Layout.selem_children(selem_l);
    [
      Dec.Profile.Selem({
        color,
        shape,
        style: Filtered,
        measurement,
        empty_holes,
        open_children,
        closed_children,
      }),
    ];
  | _ => []
  };
};

let current_space =
    (
      ~caret_mode: option(CaretMode.t)=?,
      ~measurement: Layout.measurement,
      (step, color): (Path.caret_step, Color.t),
      paths: t,
    )
    : (list(Dec.Profile.t) as 'ds) => {
  open Dec.Profile;
  let {caret, siblings, root_term: _, filtered_selems: _, logo_selems: _} = paths;
  let caret_ds =
    switch (caret, caret_mode) {
    | (Some(([], (l, _))), Some(mode)) when step == l =>
      let restructuring_ds =
        switch (mode) {
        | Restructuring({selection, _}) => [
            RestructuringGenie({
              origin: measurement.origin,
              length: Layout.length(Layout.mk_selection(color, selection)),
            }),
          ]
        | _ => []
        };
      [
        Caret({origin: measurement.origin, color, mode}),
        CaretPos({measurement, color, style: `Anchor}),
        ...restructuring_ds,
      ];
    | (Some(([], (_, r))), Some(_)) when step == r => [
        CaretPos({measurement, color, style: `Anchor}),
      ]
    | _ => []
    };
  let anchor_ds =
    switch (caret) {
    | Some(([], (l, r))) when step == l || step == r => [
        CaretPos({measurement, color, style: `Anchor}),
      ]
    | _ => []
    };
  let bare_ds =
    switch (caret_mode) {
    | None => []
    | Some(Restructuring({selection, _}))
        when !Selection.is_whole_any(selection) =>
      []
    | _ => [CaretPos({measurement, color, style: `Bare})]
    };
  let sibling_ds =
    switch (siblings) {
    | Some(([], siblings)) when List.mem(step, siblings) => [
        CaretPos({measurement, color, style: `Sibling}),
      ]
    | _ => []
    };
  List.concat([caret_ds, anchor_ds, bare_ds, sibling_ds]);
};
