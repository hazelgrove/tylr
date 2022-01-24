open Sexplib.Std;
open Util;
open Core;
open OptUtil.Syntax;

/**
 * decoration priority list:
 * - caret
 * - root_term
 * - selection highlighting
 * - tile/shard decorations
 */

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
  filtered_pieces: option((Path.steps, list(Path.piece_step))),
  revealed_pieces: option((Path.steps, list(Path.piece_step))),
  neighbor_pieces: option((Path.steps, list(Path.piece_step))),
  selection_boxes: list(Path.range),
  selection_bars: list(Path.range),
  // logo hack
  logo_pieces: list(Path.piece_step),
};

let mk =
    (
      ~caret=?,
      ~siblings=?,
      ~root_term=?,
      ~filtered_pieces=?,
      ~revealed_pieces=?,
      ~neighbor_pieces=?,
      ~selection_boxes=[],
      ~selection_bars=[],
      ~logo_pieces=[],
      (),
    ) => {
  caret,
  siblings,
  root_term,
  filtered_pieces,
  revealed_pieces,
  neighbor_pieces,
  selection_boxes,
  selection_bars,
  logo_pieces,
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
  | Restructuring((backpack, (prefix, suffix) as rframe)) =>
    if (Parser.is_backpack_whole_any(backpack)) {
      ListUtil.range(List.length(ListFrame.to_list(rframe)) + 1);
    } else {
      let (tiles_pre, prefix) =
        ListUtil.take_while(Restructuring.is_tile, prefix);
      let (tiles_suf, _) =
        ListUtil.take_while(Restructuring.is_tile, suffix);
      ListUtil.range(
        ~lo=List.length(prefix),
        List.(length(prefix) + length(tiles_pre) + length(tiles_suf) + 1),
      );
    }
  };

let filtered_pieces = (~frame_sort, subject: Subject.t) => {
  switch (subject) {
  | Pointing(_) => None
  | Selecting(_, selection, (prefix, _)) =>
    let len_pre = List.length(prefix);
    let len_sel = List.length(selection);
    Some(ListUtil.range(~lo=len_pre, len_pre + len_sel));
  | Restructuring((_, rframe)) =>
    let pieces =
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
    Some(pieces);
  };
};

let revealed_pieces = (subject: Subject.t) =>
  switch (subject) {
  | Pointing(_) => None
  | Selecting(_, selection, (prefix, _) as sframe) =>
    let len_pre = List.length(prefix);
    let len_sel = List.length(selection);
    let pieces =
      ListFrame.to_list(~subject=selection, sframe)
      |> List.mapi((i, piece) => (i, piece))
      |> List.filter_map(((i, piece)) =>
           (i < len_pre || len_pre + len_sel <= i) && Selem.is_shard(piece)
             ? Some(i) : None
         );
    Some(pieces);
  | Restructuring(_) => None
  };

let neighbor_pieces = (subject: Subject.t) =>
  switch (subject) {
  | Pointing(sframe) =>
    ListUtil.range(List.length(ListFrame.to_list(sframe)))
  | Selecting(_, selection, sframe) =>
    ListUtil.range(
      List.length(ListFrame.to_list(~subject=selection, sframe)),
    )
  | Restructuring((backpack, rframe)) =>
    if (Parser.is_backpack_whole_any(backpack)) {
      ListUtil.range(List.length(ListFrame.to_list(rframe)));
    } else {
      let (prefix, suffix) = rframe;
      let (tiles_pre, prefix) =
        ListUtil.take_while(Restructuring.is_tile, prefix);
      let (tiles_suf, _) =
        ListUtil.take_while(Restructuring.is_tile, suffix);
      ListUtil.range(
        ~lo=List.length(prefix),
        List.length(prefix @ tiles_pre @ tiles_suf),
      );
    }
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
  let filtered_pieces = {
    let+ pieces = filtered_pieces(~frame_sort=Frame.sort(frame), subject);
    (steps, pieces);
  };
  let revealed_pieces = {
    let+ pieces = revealed_pieces(subject);
    (steps, pieces);
  };
  let selections = selections(subject) |> List.map(range => (steps, range));
  mk(
    ~caret=caret_range,
    ~siblings=(steps, siblings(subject)),
    ~root_term=?root_term(zipper),
    ~filtered_pieces?,
    ~revealed_pieces?,
    ~neighbor_pieces=(steps, neighbor_pieces(subject)),
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
    filtered_pieces,
    revealed_pieces,
    neighbor_pieces,
    selection_boxes,
    selection_bars,
    logo_pieces: _,
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
    filtered_pieces: step_or_prune(filtered_pieces),
    revealed_pieces: step_or_prune(revealed_pieces),
    neighbor_pieces: step_or_prune(neighbor_pieces),
    selection_boxes:
      selection_boxes |> List.filter_map(range => step_or_prune(Some(range))),
    selection_bars:
      selection_bars |> List.filter_map(range => step_or_prune(Some(range))),
    logo_pieces: [],
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
    logo_pieces,
    selection_boxes,
    selection_bars,
    caret: _,
    siblings: _,
    filtered_pieces: _,
    revealed_pieces: _,
    neighbor_pieces: _,
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
        let (measurement, color, shape, piece_l) =
          Layout.find_piece(~origin, root_step, layout);
        Dec.Profile.Selem(
          SelemDec.Profile.of_layout(
            ~measurement,
            ~color,
            ~shape,
            ~style=Root,
            piece_l,
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
  let logo_piece_ds =
    logo_pieces
    |> List.map(step => {
         let (measurement, color, shape, _) =
           Layout.find_piece(~origin, step, layout);
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
    logo_piece_ds,
  ]);
};

let current_piece =
    (
      ~show_neighbor_tiles: bool,
      ~measurement: Layout.measurement,
      step: Path.piece_step,
      color: Color.t,
      shape: Layout.piece_shape,
      piece_l: Layout.t,
      paths: t,
    ) => {
  let {
    filtered_pieces,
    revealed_pieces,
    neighbor_pieces,
    caret: _,
    siblings: _,
    root_term: _,
    selection_boxes: _,
    selection_bars: _,
    logo_pieces: _,
  } = paths;
  let filtered_piece_ds =
    switch (filtered_pieces) {
    | Some(([], filtered_pieces)) when List.mem(step, filtered_pieces) =>
      let empty_holes = Layout.piece_holes(piece_l);
      let (open_children, closed_children) = Layout.piece_children(piece_l);
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
  let revealed_piece_ds =
    switch (revealed_pieces) {
    | Some(([], revealed_pieces)) when List.mem(step, revealed_pieces) =>
      let empty_holes = Layout.piece_holes(piece_l);
      let (open_children, closed_children) = Layout.piece_children(piece_l);
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
  let neighbor_piece_ds =
    switch (neighbor_pieces) {
    | Some(([], neighbor_pieces)) when List.mem(step, neighbor_pieces) =>
      let empty_holes = Layout.piece_holes(piece_l);
      let (open_children, closed_children) = Layout.piece_children(piece_l);
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
  List.concat([filtered_piece_ds, revealed_piece_ds, neighbor_piece_ds]);
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
    filtered_pieces: _,
    revealed_pieces: _,
    logo_pieces: _,
    neighbor_pieces: _,
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
