open Sexplib.Std;
open Util;
open Cor;

[@deriving sexp]
type tip_shape = (Tip.shape, int);
[@deriving sexp]
type selem_shape = (tip_shape, tip_shape);

[@deriving sexp]
type t =
  | Text(string)
  | Cat(t, t)
  | Annot(annot, t)
and annot =
  | Delim
  | EmptyHole(Color.t, Tip.shape)
  | Space(int, Color.t)
  | ClosedChild
  | OpenChild
  | UniChild(Sort.t, Direction.t)
  | Rail(RailStyle.t)
  | Selem({
      color: Color.t,
      shape: selem_shape,
      style: SelemStyle.t,
    })
  | Selected(Sort.t, Sort.t)
  | Step(int)
  | TargetBounds({
      sort: Sort.t,
      mode: CaretMode.t,
      strict_bounds: (bool, bool),
    });

let empty = Text("");

let annot = (annot, l) => Annot(annot, l);

let step = n => annot(Step(n));

let cat = (l1, l2) => Cat(l1, l2);
let cats =
  fun
  | [] => empty
  | [l, ...ls] => List.fold_left(cat, l, ls);

let join = (sep: t, ls: list(t)) => ls |> ListUtil.join(sep) |> cats;

let delim = s => annot(Delim, Text(s));
let empty_hole = (color, tip) =>
  annot(EmptyHole(color, tip), Text(Unicode.nbsp));
let open_child = annot(OpenChild);
let closed_child = annot(ClosedChild);
let uni_child = (~sort, ~side) => annot(UniChild(sort, side));

let space = (n, color) => Annot(Space(n, color), Text(Unicode.nbsp));
let space_sort = (n, sort) => space(n, Color.of_sort(sort));
let spaces = (~offset=0, color, ls) =>
  switch (ls) {
  | [] => empty
  | [hd, ...tl] =>
    let spaced_tl =
      tl
      |> List.mapi((i, l) => [space(offset + 1 + i, color), l])
      |> List.flatten;
    cats([hd, ...spaced_tl]);
  };
let pad = (~offset, ~len, color, l) =>
  cats([space(offset, color), l, space(offset + len, color)]);
let pad_spaces = (~offset=0, color, ls) =>
  switch (ls) {
  | [] => space(offset, color)
  | [_, ..._] =>
    pad(~offset, ~len=List.length(ls), color, spaces(color, ls))
  };

let length = {
  let rec go =
    lazy(
      Memo.memoize(
        fun
        | Text(s) => Unicode.length(s)
        | Cat(l1, l2) => Lazy.force(go, l1) + Lazy.force(go, l2)
        | Annot(_, l) => Lazy.force(go, l),
      )
    );
  Lazy.force(go);
};

type measurement = {
  start: int,
  len: int,
};

let measured_fold' =
    (
      ~text: (measurement, string) => 'acc,
      ~cat: (measurement, 'acc, 'acc) => 'acc,
      // let client cut off recursion
      ~annot: (t => 'acc, measurement, annot, t) => 'acc,
      ~start=0,
      l: t,
    ) => {
  let rec go = (~start, l: t) => {
    let m = {start, len: length(l)};
    switch (l) {
    | Text(s) => text(m, s)
    | Cat(l1, l2) =>
      let mid = start + length(l1);
      cat(m, go(~start, l1), go(~start=mid, l2));
    | Annot(ann, l) => annot(go(~start), m, ann, l)
    };
  };
  go(~start, l);
};

let measured_fold = (~annot: (measurement, annot, 'acc) => 'acc, ~start=0) =>
  measured_fold'(~annot=(k, m, ann, l) => annot(m, ann, k(l)), ~start);

let mk_Paren = body =>
  cats([
    delim("("),
    open_child(step(ChildStep.paren_body, body)),
    delim(")"),
  ]);

let mk_Lam = p =>
  cats([
    delim(Unicode.lam),
    closed_child(step(ChildStep.lam_pat, p)),
    delim("."),
  ]);

let mk_Let = (p, def) => {
  cats([
    delim("let"),
    closed_child(step(ChildStep.let_pat, p)),
    delim("="),
    open_child(step(ChildStep.let_def, def)),
    delim("in"),
  ]);
};

let mk_Plus = () => delim("+");
let mk_Times = () => delim("*");

let mk_Prod = () => delim(",");

let mk_OpHole = empty_hole;
let mk_BinHole = empty_hole;

let mk_text = s => Text(s);

let mk_token =
  Shard.get(
    fun
    | Shard_pat.Paren_l => delim("(")
    | Paren_r => delim(")"),
    fun
    | Shard_exp.Paren_l => delim("(")
    | Paren_r => delim(")")
    | Lam_lam => delim(Unicode.lam)
    | Lam_dot => delim(".")
    | Let_let => delim("let")
    | Let_eq => delim("=")
    | Let_in => delim("in"),
  );

let is_atomic = (t: Tile.t) =>
  [] == Parser.disassemble_selem(Right, Tile(t));

let rec mk_tiles = (~offset=0, ~rail_color=?, ts) =>
  List.mapi(
    (i, tile) => {
      let l_tile = mk_tile(tile);
      let with_rail =
        switch (rail_color) {
        | None => l_tile
        | Some(color) =>
          annot(Rail({color, atomic: is_atomic(tile)}), l_tile)
        };
      step(offset + i, with_rail);
    },
    ts,
  )
and mk_tile = t =>
  t
  |> Tile.get(
       fun
       | Tile_pat.OpHole => mk_OpHole(Pat, Convex)
       | Var(x) => mk_text(x)
       | Paren(body) =>
         // TODO undo unnecessary rewrapping
         mk_Paren(pad_spaces(Pat, mk_tiles(Tiles.of_pat(body))))
       | BinHole => mk_BinHole(Pat, Concave)
       | Prod => mk_Prod(),
       fun
       | Tile_exp.OpHole => mk_OpHole(Exp, Convex)
       | Num(n) => mk_text(string_of_int(n))
       | Var(x) => mk_text(x)
       | Paren(body) =>
         mk_Paren(pad_spaces(Exp, mk_tiles(Tiles.of_exp(body))))
       | Lam(p) => mk_Lam(pad_spaces(Pat, mk_tiles(Tiles.of_pat(p))))
       | Let(p, def) =>
         mk_Let(
           pad_spaces(Pat, mk_tiles(Tiles.of_pat(p))),
           pad_spaces(Exp, mk_tiles(Tiles.of_exp(def))),
         )
       | BinHole => mk_BinHole(Exp, Concave)
       | Plus => mk_Plus()
       | Times => mk_Times()
       | Prod => mk_Prod(),
     );

let selem_shape = selem => {
  let (lshape, _) = Selem.tip(Left, selem);
  let (rshape, _) = Selem.tip(Right, selem);
  let ltails = Selem.tails(Left, selem);
  let rtails = Selem.tails(Right, selem);
  ((lshape, ltails), (rshape, rtails));
};

let mk_selem =
    (~style: option(SelemStyle.t)=?, color: Color.t, selem: Selem.t) => {
  let l = selem |> Selem.get(mk_token, mk_tile);
  switch (style) {
  | None => l
  | Some(style) =>
    let shape = selem_shape(selem);
    annot(Selem({color, shape, style}), l);
  };
};
let mk_selection =
    (
      ~offset=0,
      ~style: option(SelemStyle.t)=?,
      frame_color: Color.t,
      selection,
    ) =>
  switch (Selection.tip_sorts(selection)) {
  | None => space(offset, frame_color)
  | Some((sort_l, sort_r)) =>
    selection
    |> List.mapi((i, selem) =>
         [
           step(
             offset + i,
             mk_selem(~style?, Color.of_sort(Selem.sort(selem)), selem),
           ),
           space(
             offset + i + 1,
             Color.of_sort(snd(Selem.tip(Right, selem))),
           ),
         ]
       )
    |> List.flatten
    |> List.cons(space(offset, Color.of_sort(sort_l)))
    |> cats
    |> annot(Selected(sort_l, sort_r))
  };

let mk_root_tile = (~offset, ~sort, tile) =>
  step(
    offset,
    annot(
      Selem({
        color: Color.of_sort(sort),
        shape: selem_shape(Tile(tile)),
        style: Root,
      }),
      mk_tile(tile),
    ),
  );

let zip_up =
    (subject: Selection.t, frame: Frame.t)
    : option((Tile.t, t, ListFrame.t(Tile.t), Frame.t)) => {
  let mk_ts = (~show_rails, sort, ts) => {
    let rail_color = show_rails ? Some(Color.of_sort(sort)) : None;
    pad_spaces(Color.of_sort(sort), mk_tiles(~rail_color?, ts));
  };
  let mk_bounded_ts = (sort, ts) =>
    annot(
      TargetBounds({sort, mode: Pointing, strict_bounds: (false, false)}),
      mk_ts(~show_rails=true, sort, ts),
    );
  let mk_bounded_pat = selection => {
    let ts = Option.get(Selection.get_tiles(selection));
    (Option.get(Tiles.get_pat(ts)), mk_bounded_ts(Pat, ts));
  };
  let mk_bounded_exp = selection => {
    let ts = Option.get(Selection.get_tiles(selection));
    (Option.get(Tiles.get_exp(ts)), mk_bounded_ts(Exp, ts));
  };
  let mk_root_tile = (~tile_step, ~tile, l) =>
    step(
      tile_step,
      annot(
        Selem({
          shape: selem_shape(Tile(tile)),
          color: Color.of_sort(Tile.sort(tile)),
          style: Root,
        }),
        l,
      ),
    );
  switch (frame) {
  | Pat(Paren_body((tframe, frame))) =>
    let tile_step = List.length(fst(tframe));
    let (body, l_body) = mk_bounded_pat(subject);
    let tframe = TupleUtil.map2(Tiles.of_pat, tframe);
    let tile = Tile.Pat(Paren(body));
    Some((
      tile,
      mk_root_tile(~tile_step, ~tile, mk_Paren(l_body)),
      tframe,
      Pat(frame),
    ));
  | Pat(Lam_pat((tframe, frame))) =>
    let tile_step = List.length(fst(tframe));
    let (p, l_p) = mk_bounded_pat(subject);
    let tframe = TupleUtil.map2(Tiles.of_exp, tframe);
    let tile = Tile.Exp(Lam(p));
    Some((
      tile,
      mk_root_tile(~tile_step, ~tile, mk_Lam(l_p)),
      tframe,
      Exp(frame),
    ));
  | Pat(Let_pat(def, (tframe, frame))) =>
    let tile_step = List.length(fst(tframe));
    let (p, l_p) = mk_bounded_pat(subject);
    let l_def = mk_ts(~show_rails=false, Exp, Tiles.of_exp(def));
    let tframe = TupleUtil.map2(Tiles.of_exp, tframe);
    let tile = Tile.Exp(Let(p, def));
    Some((
      tile,
      mk_root_tile(~tile_step, ~tile, mk_Let(l_p, l_def)),
      tframe,
      Exp(frame),
    ));
  | Exp(Paren_body((tframe, frame))) =>
    let tile_step = List.length(fst(tframe));
    let (body, l_body) = mk_bounded_exp(subject);
    let tframe = TupleUtil.map2(Tiles.of_exp, tframe);
    let tile = Tile.Exp(Paren(body));
    Some((
      tile,
      mk_root_tile(~tile_step, ~tile, mk_Paren(l_body)),
      tframe,
      Exp(frame),
    ));
  | Exp(Let_def(p, (tframe, frame))) =>
    let tile_step = List.length(fst(tframe));
    let (def, l_def) = mk_bounded_exp(subject);
    let l_p = mk_ts(~show_rails=false, Pat, Tiles.of_pat(p));
    let tframe = TupleUtil.map2(Tiles.of_exp, tframe);
    let tile = Tile.Exp(Let(p, def));
    Some((
      tile,
      mk_root_tile(~tile_step, ~tile, mk_Let(l_p, l_def)),
      tframe,
      Exp(frame),
    ));
  | Exp(Root) => None
  };
};

let get_uni_children =
    (root_tile: Tile.t, (prefix, _) as tframe: ListFrame.t(Tile.t))
    : (ListFrame.t(Tile.t), ListFrame.t(Tile.t)) => {
  let tiles = ListFrame.to_list(~subject=[root_tile], tframe);
  let skel = Parser.associate(tiles);
  let m = List.length(prefix);
  let subskel = Skel.skel_at(List.length(prefix), skel);
  let (n, _) as range = Skel.range(subskel);
  let (subtiles, tframe) = ListFrame.split_sublist(range, tiles);
  let (_, uni_children) = ListFrame.split_nth(m - n, subtiles);
  (uni_children, tframe);
};

let rec mk_frame = (subject: t, frame: Frame.t): t => {
  let mk_tiles_pat = (~offset=0, ts) =>
    mk_tiles(~offset, List.map(Tile.pat, ts));
  let mk_tiles_exp = (~offset=0, ts) =>
    mk_tiles(~offset, List.map(Tile.exp, ts));
  let mk_frame_pat = (tile, ((prefix, suffix), frame): Frame_pat.s) => {
    let n = List.length(prefix);
    let ls_prefix = mk_tiles_pat(List.rev(prefix));
    let ls_suffix = mk_tiles_pat(~offset=n + 1, suffix);
    mk_frame(
      pad_spaces(Pat, ls_prefix @ [step(n, tile), ...ls_suffix]),
      Pat(frame),
    );
  };
  let mk_frame_exp = (tile, ((prefix, suffix), frame): Frame_exp.s) => {
    let n = List.length(prefix);
    let ls_prefix = mk_tiles_exp(List.rev(prefix));
    let ls_suffix = mk_tiles_exp(~offset=n + 1, suffix);
    mk_frame(
      pad_spaces(Exp, ls_prefix @ [step(n, tile), ...ls_suffix]),
      Exp(frame),
    );
  };
  switch (frame) {
  | Pat(Paren_body(frame_s)) =>
    let tile = mk_Paren(subject);
    mk_frame_pat(tile, frame_s);
  | Pat(Lam_pat(frame_s)) =>
    let tile = mk_Lam(subject);
    mk_frame_exp(tile, frame_s);
  | Pat(Let_pat(def, frame_s)) =>
    let tile = mk_Let(subject, pad_spaces(Exp, mk_tiles_exp(def)));
    mk_frame_exp(tile, frame_s);
  | Exp(Paren_body(frame_s)) =>
    let tile = mk_Paren(subject);
    mk_frame_exp(tile, frame_s);
  | Exp(Let_def(p, frame_s)) =>
    let tile = mk_Let(pad_spaces(Pat, mk_tiles_pat(p)), subject);
    mk_frame_exp(tile, frame_s);
  | Exp(Root) => subject
  };
};

let mk_pointing = (sframe: Selection.frame, frame: Frame.t): t => {
  let mk_subject =
      (
        ~sort: Sort.t,
        ~show_rails: bool,
        l_root_tile: t,
        (child_pre, child_suf): ListFrame.t(Tile.t),
        (prefix, suffix): ListFrame.t(Tile.t),
      ) => {
    let c = Color.of_sort(sort);
    let uni_child = uni_child(~sort);

    let mk_tile = tile => {
      let l = mk_tile(tile);
      show_rails ? annot(Rail({color: c, atomic: is_atomic(tile)}), l) : l;
    };

    let mk_prefix = (offset, prefix) =>
      prefix
      |> List.rev
      |> List.mapi((i, tile) => {
           [space(offset + i, c), step(offset + i, mk_tile(tile))]
         })
      |> List.flatten;
    let mk_suffix = (offset, suffix) =>
      suffix
      |> List.mapi((i, tile) =>
           [step(offset + i, mk_tile(tile)), space(offset + i + 1, c)]
         )
      |> List.flatten;

    let offset = 0;
    let l_prefix = mk_prefix(offset, prefix);

    let offset = offset + List.length(prefix);
    let l_child_pre = {
      let ls = mk_prefix(offset, child_pre);
      switch (ls) {
      | [] => []
      | [_, ..._] => [uni_child(~side=Left, cats(ls))]
      };
    };

    let offset = offset + List.length(child_pre);
    let l_root_tile = [space(offset, c), l_root_tile, space(offset + 1, c)];

    let offset = offset + 1;
    let l_child_suf = {
      let ls = mk_suffix(offset, child_suf);
      switch (ls) {
      | [] => []
      | [_, ..._] => [uni_child(~side=Right, cats(ls))]
      };
    };

    let offset = offset + List.length(child_suf);
    let l_suffix = mk_suffix(offset, suffix);

    cats(
      List.concat([
        l_prefix,
        l_child_pre,
        l_root_tile,
        l_child_suf,
        l_suffix,
      ]),
    );
  };

  let tframe =
    sframe
    |> TupleUtil.map2(Selection.get_tiles)
    |> TupleUtil.map2(
         OptUtil.get_or_fail("expected prefix/suffix to consist of tiles"),
       );
  let frame_sort = Frame.sort(frame);
  let frame_color = Color.of_sort(frame_sort);
  switch (tframe) {
  | (prefix, []) =>
    switch (zip_up(Selection.of_tiles(List.rev(prefix)), frame)) {
    | None =>
      let (root_tile, prefix) = ListUtil.split_first(prefix);
      let (uni_children, tframe) =
        get_uni_children(root_tile, (prefix, []));
      let l_root_tile =
        mk_root_tile(
          ~offset=List.length(prefix),
          ~sort=frame_sort,
          root_tile,
        )
        |> annot(Rail({color: frame_color, atomic: is_atomic(root_tile)}));
      mk_frame(
        mk_subject(
          ~sort=frame_sort,
          ~show_rails=true,
          l_root_tile,
          uni_children,
          tframe,
        )
        |> annot(
             TargetBounds({
               sort: frame_sort,
               mode: Pointing,
               strict_bounds: (true, true),
             }),
           ),
        frame,
      );
    | Some((root_tile, l_root_tile, tframe, outer_frame)) =>
      let (uni_children, tframe) = get_uni_children(root_tile, tframe);
      mk_frame(
        mk_subject(
          ~sort=Frame.sort(outer_frame),
          ~show_rails=false,
          l_root_tile,
          uni_children,
          tframe,
        ),
        outer_frame,
      );
    }
  | (prefix, [root_tile, ...suffix]) =>
    let (uni_children, tframe) =
      get_uni_children(root_tile, (prefix, suffix));
    let l_root_tile =
      mk_root_tile(~offset=List.length(prefix), ~sort=frame_sort, root_tile)
      |> annot(Rail({color: frame_color, atomic: is_atomic(root_tile)}));
    let is_root = Frame.is_root(frame);
    mk_frame(
      mk_subject(
        ~sort=Frame.sort(frame),
        ~show_rails=true,
        l_root_tile,
        uni_children,
        tframe,
      )
      |> annot(
           TargetBounds({
             sort: frame_sort,
             mode: Pointing,
             strict_bounds: (is_root, is_root),
           }),
         ),
      frame,
    );
  };
};

let mk_affix =
    (
      ~restructuring_partial: bool,
      ~reveal_tiles: bool,
      ~show_children: bool,
      ~sort: Sort.t,
      ~offset: int,
      d: Direction.t,
      selems,
    ) => {
  switch (selems) {
  | [] => empty
  | [_, ..._] =>
    let rev = l => d == Left ? List.rev(l) : l;
    let mk_selem = (~with_rail, (i, selem)) => {
      let color = Color.of_sort(Selem.sort(selem));
      let l_selem =
        mk_selem(
          ~style=?
            if (!Selection.filter_pred(sort, selem)) {
              Some(Filtered);
            } else if (reveal_tiles) {
              Some(Revealed({show_children: show_children}));
            } else {
              None;
            },
          color,
          selem,
        )
        |> (l => with_rail ? annot(Rail({color, atomic: true}), l) : l)
        |> annot(Step(i));
      let l_space =
        space(
          d == Left ? i : i + 1,
          Color.of_sort(snd(Selem.tip(d, selem))),
        );
      [l_selem, l_space];
    };
    let (ts, selems) =
      selems
      |> rev
      |> List.mapi((i, selem) => (offset + i, selem))
      |> rev
      |> ListUtil.take_while(((_, selem)) => Selem.is_tile(selem));
    let l_ts = ts |> List.map(mk_selem(~with_rail=true)) |> List.flatten;
    let l_selems =
      selems
      |> List.map(mk_selem(~with_rail=!restructuring_partial))
      |> List.flatten;
    cats(rev(l_ts @ l_selems));
  };
};

let mk_selecting =
    (
      selection: Selection.t,
      (prefix, suffix): Selection.frame,
      frame: Frame.t,
    ) => {
  let reveal_tiles = selection == [];
  let sort_frame = Frame.sort(frame);
  let mk_affix =
    mk_affix(~reveal_tiles, ~show_children=true, ~sort=sort_frame);

  let offset = 0;
  let l_prefix =
    mk_affix(~restructuring_partial=false, ~offset, Left, prefix);

  let offset = offset + List.length(prefix);
  let l_selection =
    mk_selection(
      ~offset,
      ~style=Filtered,
      Color.of_sort(sort_frame),
      selection,
    );

  let offset = offset + List.length(selection);
  let l_suffix =
    mk_affix(~restructuring_partial=false, ~offset, Right, suffix);

  let subject = cats([l_prefix, l_selection, l_suffix]);
  mk_frame(subject, frame);
};

let mk_restructuring =
    (
      selection: Selection.t,
      (prefix, suffix): Selection.frame,
      frame: Frame.t,
    ) => {
  let sort_frame = Frame.sort(frame);
  let mk_affix =
    mk_affix(
      ~restructuring_partial=!Selection.is_whole_any(selection),
      ~reveal_tiles=false,
      ~show_children=Selection.is_whole_any(selection),
      ~sort=sort_frame,
    );
  // let selection =
  //   switch (selection) {
  //   | [] => []
  //   | [hd, ..._] =>
  //     let c = Color.of_sort(snd(Selem.tip(Left, hd)));
  //     let selems =
  //       mk_selection(~style=Selected, Selected, selection);
  //     [annot(Selected, pad(c, spaces(Selected, selems)))];
  //   };

  let offset = 0;
  let l_prefix = mk_affix(~offset, Left, prefix);

  let offset = offset + List.length(prefix);
  let l_suffix = mk_affix(~offset, Right, suffix);

  let s =
    switch (prefix) {
    | [] => sort_frame
    | [hd, ..._] => snd(Selem.tip(Right, hd))
    };
  let subject = cats([l_prefix, space_sort(offset, s), l_suffix]);

  mk_frame(subject, frame);
};

let mk_zipper = (zipper: Zipper.t) =>
  switch (zipper) {
  | (Pointing(sframe), frame) => mk_pointing(sframe, frame)
  | (Selecting(selection, sframe), frame) =>
    mk_selecting(selection, sframe, frame)
  | (Restructuring(selection, sframe), frame) =>
    mk_restructuring(selection, sframe, frame)
  };