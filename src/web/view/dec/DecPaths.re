open Sexplib.Std;
open Util;
open Core;
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
  revealed_selems: option((Path.steps, list(Path.selem_step))),
  neighbor_selems: option((Path.steps, list(Path.selem_step))),
  selection_boxes: list(Path.range),
  selection_bars: list(Path.range),
  // logo hack
  logo_selems: list(Path.selem_step),
};

let mk =
    (
      ~caret=?,
      ~siblings=?,
      ~root_term=?,
      ~filtered_selems=?,
      ~revealed_selems=?,
      ~neighbor_selems=?,
      ~selection_boxes=[],
      ~selection_bars=[],
      ~logo_selems=[],
      (),
    ) => {
  caret,
  siblings,
  root_term,
  filtered_selems,
  revealed_selems,
  neighbor_selems,
  selection_boxes,
  selection_bars,
  logo_selems,
};
let empty = mk();

let root_term = ((subject, frame): Zipper.t) => {
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
        switch (Zipper.zip_up(Selection.of_tiles(List.rev(prefix)), frame)) {
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
  | Selecting(_, selection, (prefix, suffix)) =>
    let len_pre = List.length(prefix);
    let len_sel = List.length(selection);
    let len_suf = List.length(suffix);
    ListUtil.range(len_pre + 1)
    @ ListUtil.range(~lo=len_pre + len_sel, len_pre + len_sel + len_suf + 1);
  | Restructuring((_backpack, (_prefix, _suffix) as rframe)) =>
    ListUtil.range(List.length(ListFrame.to_list(rframe)) + 1)
  // if (Parser.is_backpack_whole_any(backpack)) {
  //   ListUtil.range(List.length(ListFrame.to_list(rframe)) + 1);
  // } else {
  //   let (tiles_pre, prefix) =
  //     ListUtil.take_while(Restructuring.is_tile, prefix);
  //   let (tiles_suf, _) =
  //     ListUtil.take_while(Restructuring.is_tile, suffix);
  //   ListUtil.range(
  //     ~lo=List.length(prefix),
  //     List.(length(prefix) + length(tiles_pre) + length(tiles_suf) + 1),
  //   );
  // }
  };

let filtered_selems = (~frame_sort, subject: Subject.t) => {
  switch (subject) {
  | Pointing(_) => None
  | Selecting(_, selection, (prefix, _)) =>
    let len_pre = List.length(prefix);
    let len_sel = List.length(selection);
    Some(ListUtil.range(~lo=len_pre, len_pre + len_sel));
  | Restructuring((_, rframe)) =>
    let selems =
      ListFrame.to_list(rframe)
      |> ListUtil.fold_left_map(
           (step, relem) =>
             (
               step + Restructuring.len_elem(relem),
               switch (relem) {
               | Tile(tile) => Tile.sort(tile) == frame_sort ? [] : [step]
               | Selection(selection) =>
                 ListUtil.range(~lo=step, step + List.length(selection))
               },
             ),
           0,
         )
      |> snd
      |> List.flatten;
    Some(selems);
  };
};

let revealed_selems = (subject: Subject.t) =>
  switch (subject) {
  | Pointing(_) => None
  | Selecting(_, selection, (prefix, _) as sframe) =>
    let len_pre = List.length(prefix);
    let len_sel = List.length(selection);
    let selems =
      ListFrame.to_list(~subject=selection, sframe)
      |> List.mapi((i, selem) => (i, selem))
      |> List.filter_map(((i, selem)) =>
           (i < len_pre || len_pre + len_sel <= i) && Selem.is_shard(selem)
             ? Some(i) : None
         );
    Some(selems);
  | Restructuring(_) => None
  };

let neighbor_selems = (subject: Subject.t) =>
  switch (subject) {
  | Pointing(sframe) =>
    ListUtil.range(List.length(ListFrame.to_list(sframe)))
  | Selecting(_, selection, sframe) =>
    ListUtil.range(
      List.length(ListFrame.to_list(~subject=selection, sframe)),
    )
  | Restructuring((_backpack, rframe)) =>
    ListUtil.range(List.length(ListFrame.to_list(rframe)))
  // if (Parser.is_backpack_whole_any(backpack)) {
  //   ListUtil.range(List.length(ListFrame.to_list(rframe)));
  // } else {
  //   let (prefix, suffix) = rframe;
  //   let (tiles_pre, prefix) =
  //     ListUtil.take_while(Restructuring.is_tile, prefix);
  //   let (tiles_suf, _) =
  //     ListUtil.take_while(Restructuring.is_tile, suffix);
  //   ListUtil.range(
  //     ~lo=List.length(prefix),
  //     List.length(prefix @ tiles_pre @ tiles_suf),
  //   );
  // }
  };

let selections = (subject: Subject.t) =>
  switch (subject) {
  | Pointing(_) => []
  | Selecting(_, selection, (prefix, _)) =>
    let len_pre = List.length(prefix);
    let len_sel = List.length(selection);
    [(len_pre, len_pre + len_sel)];
  | Restructuring((_, rframe)) =>
    ListFrame.to_list(rframe)
    |> ListUtil.fold_left_map(
         (step, relem) =>
           (
             step + Restructuring.len_elem(relem),
             switch (relem) {
             | Tile(_) => None
             | Selection(selection) =>
               Some((step, step + List.length(selection)))
             },
           ),
         0,
       )
    |> snd
    |> List.filter_map(x => x)
  };

let of_zipper = ((subject, frame): Zipper.t) => {
  // hack to show pointing mode decorations
  // in selecting mode with empty selection
  let subject =
    switch (subject) {
    | Selecting(_, [], sframe) => Subject.Pointing(sframe)
    | _ => subject
    };
  let zipper = (subject, frame);
  let (steps, _) as caret_range = Path.mk_range(zipper);
  let filtered_selems = {
    let+ selems = filtered_selems(~frame_sort=Frame.sort(frame), subject);
    (steps, selems);
  };
  let revealed_selems = {
    let+ selems = revealed_selems(subject);
    (steps, selems);
  };
  let selections = selections(subject) |> List.map(range => (steps, range));
  mk(
    ~caret=caret_range,
    ~siblings=(steps, siblings(subject)),
    ~root_term=?root_term(zipper),
    ~filtered_selems?,
    ~revealed_selems?,
    ~neighbor_selems=(steps, neighbor_selems(subject)),
    ~selection_boxes=selections,
    ~selection_bars=selections,
    (),
  );
};

let take_two_step = (two_step: Path.two_step, paths: t): t => {
  let {
    caret,
    siblings,
    root_term,
    filtered_selems,
    revealed_selems,
    neighbor_selems,
    selection_boxes,
    selection_bars,
    logo_selems: _,
  } = paths;
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
    revealed_selems: step_or_prune(revealed_selems),
    neighbor_selems: step_or_prune(neighbor_selems),
    selection_boxes:
      selection_boxes |> List.filter_map(range => step_or_prune(Some(range))),
    selection_bars:
      selection_bars |> List.filter_map(range => step_or_prune(Some(range))),
    logo_selems: [],
  };
  paths;
};

// TODO have everything be computed from here
let current_bidelimited =
    (
      ~show_neighbor_tiles: bool,
      ~origin: int,
      ~sort: Sort.t,
      layout: Layout.t,
      paths: t,
    ) => {
  let {
    root_term,
    logo_selems,
    selection_boxes,
    selection_bars,
    caret: _,
    siblings: _,
    filtered_selems: _,
    revealed_selems: _,
    neighbor_selems: _,
  } = paths;
  let selection_box_ds =
    selection_boxes
    |> List.filter_map(((steps, range)) =>
         switch (steps) {
         | [_, ..._] => None
         | [] =>
           let (_, measurement) = Layout.find_range(~origin, range, layout);
           Some(Dec.Profile.[SelectedBox(measurement)]);
         }
       )
    |> List.flatten;
  let selection_bar_ds =
    selection_bars
    |> List.filter_map(((steps, range)) =>
         switch (steps) {
         | [_, ..._] => None
         | [] =>
           let (ends, measurement) =
             Layout.find_range(~origin, range, layout);
           Some(Dec.Profile.[SelectedBar({ends, measurement})]);
         }
       )
    |> List.flatten;

  let root_term_ds =
    switch (root_term) {
    | Some(([], skel)) when !show_neighbor_tiles =>
      let root_tile_profile = {
        let root_step = Skel.root_index(skel);
        let (measurement, color, shape, selem_l) =
          Layout.find_selem(~origin, root_step, layout);
        Dec.Profile.Selem(
          SelemDec.Profile.of_layout(
            ~measurement,
            ~color,
            ~shape,
            ~style=Root,
            selem_l,
          ),
        );
      };
      let uni_child_profiles =
        Skel.children(skel)
        |> List.map(((side, skel)) => {
             let range = Skel.range(skel);
             let (_, measurement) =
               Layout.find_range(~origin, range, layout);
             Dec.Profile.UniChild({measurement, sort, side});
           });
      [root_tile_profile, ...uni_child_profiles];
    | _ => []
    };
  let logo_selem_ds =
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
  List.concat([
    selection_box_ds,
    selection_bar_ds,
    root_term_ds,
    logo_selem_ds,
  ]);
};

let current_selem =
    (
      ~show_neighbor_tiles: bool,
      ~measurement: Layout.measurement,
      step: Path.selem_step,
      color: Color.t,
      shape: Layout.selem_shape,
      selem_l: Layout.t,
      paths: t,
    ) => {
  let {
    filtered_selems,
    revealed_selems,
    neighbor_selems,
    caret: _,
    siblings: _,
    root_term: _,
    selection_boxes: _,
    selection_bars: _,
    logo_selems: _,
  } = paths;
  let filtered_selem_ds =
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
  let revealed_selem_ds =
    switch (revealed_selems) {
    | Some(([], revealed_selems)) when List.mem(step, revealed_selems) =>
      let empty_holes = Layout.selem_holes(selem_l);
      let (open_children, closed_children) = Layout.selem_children(selem_l);
      [
        Dec.Profile.Selem({
          color,
          shape,
          style: Revealed({show_children: true}),
          measurement,
          empty_holes,
          open_children,
          closed_children,
        }),
      ];
    | _ => []
    };
  let neighbor_selem_ds =
    switch (neighbor_selems) {
    | Some(([], neighbor_selems)) when List.mem(step, neighbor_selems) =>
      let empty_holes = Layout.selem_holes(selem_l);
      let (open_children, closed_children) = Layout.selem_children(selem_l);
      [
        Dec.Profile.Rail({measurement, color}),
        ...show_neighbor_tiles
             ? [
               Dec.Profile.Selem({
                 color,
                 shape,
                 style: Revealed({show_children: true}),
                 measurement,
                 empty_holes,
                 open_children,
                 closed_children,
               }),
             ]
             : [],
      ];
    | _ => []
    };
  List.concat([filtered_selem_ds, revealed_selem_ds, neighbor_selem_ds]);
};

let current_space =
    (
      ~delete_actions: option(_)=?,
      ~caret_mode: option(CaretMode.t)=?,
      ~measurement: Layout.measurement,
      ~just_failed: option(FailedInput.t),
      (step, color): (Path.caret_step, Color.t),
      paths: t,
    )
    : (list(Dec.Profile.t) as 'ds) => {
  open Dec.Profile;
  let {
    caret,
    siblings,
    selection_bars,
    selection_boxes: _,
    root_term: _,
    filtered_selems: _,
    revealed_selems: _,
    logo_selems: _,
    neighbor_selems: _,
  } = paths;
  let delete_actions =
    switch (delete_actions) {
    | None => (None, None)
    | Some(das) => das
    };
  let caret_ds =
    switch (caret, caret_mode) {
    | (
        Some(([], (l, _))),
        Some((Pointing | Selecting(Left, _) | Restructuring(_)) as mode),
      )
        when step == l =>
      let restructuring_ds =
        switch (mode) {
        | Restructuring({backpack: (_d_todo, selection, _rest_todo), _}) => [
            RestructuringGenie({
              origin: measurement.origin,
              length:
                Layout.length(
                  Layout.mk_selection(~frame_color=color, selection),
                ),
            }),
          ]
        | _ => []
        };
      [
        Caret({
          delete_actions,
          origin: measurement.origin,
          color,
          mode,
          just_failed,
        }),
        CaretPos({measurement, color, just_failed, style: `Caret}),
        ...restructuring_ds,
      ];
    | (
        Some(([], (_, r))),
        Some((Pointing | Selecting(Right, _) | Restructuring(_)) as mode),
      )
        when step == r => [
        Caret({
          delete_actions,
          origin: measurement.origin,
          color,
          mode,
          just_failed,
        }),
        CaretPos({measurement, color, just_failed, style: `Caret}),
      ]
    | _ => []
    };
  let anchor_ds = {
    let is_relevant_to_step = ((l, r)) => step == l || step == r;
    let caret_ds =
      switch (caret) {
      | Some(([], range)) when is_relevant_to_step(range) => [
          CaretPos({measurement, color, just_failed: None, style: `Anchor}),
        ]
      | _ => []
      };
    let selection_ds =
      if (selection_bars
          |> List.exists(((steps, range)) =>
               steps == [] && is_relevant_to_step(range)
             )) {
        [CaretPos({measurement, color, just_failed: None, style: `Anchor})];
      } else {
        [];
      };
    caret_ds @ selection_ds;
  };
  let bare_ds =
    switch (caret_mode) {
    | None => []
    | Some(Restructuring({backpack, _}))
        when !Parser.is_backpack_whole_any(backpack) =>
      []
    | _ => [CaretPos({measurement, color, just_failed: None, style: `Bare})]
    };
  let sibling_ds =
    switch (siblings) {
    | Some(([], siblings)) when List.mem(step, siblings) => [
        CaretPos({measurement, color, just_failed: None, style: `Sibling}),
      ]
    | _ => []
    };
  List.concat([caret_ds, anchor_ds, bare_ds, sibling_ds]);
};
