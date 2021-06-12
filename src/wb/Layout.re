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
  | EmptyHole(Color.t)
  | Space(int, Color.t)
  | ClosedChild
  | OpenChild
  | UniChild(Sort.t, Direction.t)
  | Selem({
      color: Color.t,
      shape: selem_shape,
      style: SelemStyle.t,
      atomic: bool,
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
let empty_hole = color => annot(EmptyHole(color), Text(Unicode.nbsp));
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
// let pad_spaces_z = (ls_pre, caret, ls_suf) => {
//   let caret = space(~caret, ());
//   switch (ls_pre, ls_suf) {
//   | ([], []) => caret
//   | ([], [_, ..._]) => cats([caret, spaces(ls_suf), space()])
//   | ([_, ..._], []) => cats([space(), spaces(ls_pre), caret])
//   | ([_, ..._], [_, ..._]) =>
//     pad(cats([spaces(ls_pre), caret, spaces(ls_suf)]))
//   };
// };

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

// let rec place_caret = (d: Direction.t, (sort, caret), l) =>
//   switch (l) {
//   | Text(_) => l
//   | Cat(l1, l2) =>
//     switch (d) {
//     | Left => Cat(place_caret(d, (sort, caret), l1), l2)
//     | Right => Cat(l1, place_caret(d, (sort, caret), l2))
//     }
//   | Annot(Space(_), l) => Annot(Space(Some((sort, caret))), l)
//   | Annot(annot, l) => Annot(annot, place_caret(d, (sort, caret), l))
//   };

// type with_dangling_caret = (t, option(Direction.t));

// let place_caret_0:
//   option((Sort.t, caret, CaretPosition.t)) => option(Direction.t) =
//   Option.map(
//     fun
//     | (_, _, CaretPosition.Before(_)) => Direction.Left
//     | (_, _, After) => Right,
//   );
// let place_caret_1 = (caret, child1) =>
//   switch (caret) {
//   | None => (child1, None)
//   | Some((_, _, CaretPosition.Before(0))) => (child1, Some(Direction.Left))
//   | Some((sort, caret, Before(_one))) => (
//       place_caret(Right, (sort, caret), child1),
//       None,
//     )
//   | Some((_, _, After)) => (child1, Some(Right))
//   };
// let place_caret_2 = (caret, child1, child2) =>
//   switch (caret) {
//   | None => (child1, child2, None)
//   | Some((_, _, CaretPosition.Before(0))) => (
//       child1,
//       child2,
//       Some(Direction.Left),
//     )
//   | Some((sort, caret, Before(1))) => (
//       place_caret(Right, (sort, caret), child1),
//       child2,
//       None,
//     )
//   | Some((sort, caret, Before(_two))) => (
//       child1,
//       place_caret(Right, (sort, caret), child2),
//       None,
//     )
//   | Some((_, _, After)) => (child1, child2, Some(Right))
//   };

let mk_Paren = body =>
  cats([delim("("), open_child(step(0, body)), delim(")")]);

let mk_Lam = p =>
  cats([delim(Unicode.lam), closed_child(step(0, p)), delim(".")]);

let mk_Let = (p, def) => {
  cats([
    delim("let"),
    closed_child(step(0, p)),
    delim("="),
    open_child(step(1, def)),
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
  Token.get(
    fun
    | Token_pat.Paren_l => delim("(")
    | Paren_r => delim(")"),
    fun
    | Token_exp.Paren_l => delim("(")
    | Paren_r => delim(")")
    | Lam_lam => delim(Unicode.lam)
    | Lam_dot => delim(".")
    | Let_let => delim("let")
    | Let_eq => delim("=")
    | Let_in => delim("in"),
  );

let rec mk_tiles = (~offset=0, ts) =>
  List.mapi((i, t) => step(offset + i, mk_tile(t)), ts)
and mk_tile = t =>
  t
  |> Tile.get(
       fun
       | Tile_pat.OpHole => mk_OpHole(Pat)
       | Var(x) => mk_text(x)
       | Paren(body) =>
         // TODO undo unnecessary rewrapping
         mk_Paren(pad_spaces(Pat, mk_tiles(Tiles.of_pat(body))))
       | BinHole => mk_BinHole(Pat)
       | Prod => mk_Prod(),
       fun
       | Tile_exp.OpHole => mk_OpHole(Exp)
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
       | BinHole => mk_BinHole(Exp)
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
    let atomic = Parser.disassemble_selem(Right, selem) == [];
    annot(Selem({color, shape, style, atomic}), l);
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
           step(offset + i, mk_selem(~style?, Selected, selem)),
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

let is_atomic = (t: Tile.t) =>
  [] == Parser.disassemble_selem(Right, Tile(t));
let mk_root_tile = (~offset, ~sort, tile) =>
  step(
    offset,
    annot(
      Selem({
        color: Color.of_sort(sort),
        shape: selem_shape(Tile(tile)),
        style: Root,
        atomic: is_atomic(tile),
      }),
      mk_tile(tile),
    ),
  );

let zip_up =
    (subject: Selection.t, frame: Frame.t)
    : option((Tile.t, t, ListFrame.t(Tile.t), Frame.t)) => {
  let mk_ts = (sort, ts) => pad_spaces(Color.of_sort(sort), mk_tiles(ts));
  let mk_bounded_ts = (sort, ts) =>
    annot(
      TargetBounds({sort, mode: Pointing, strict_bounds: (false, false)}),
      mk_ts(sort, ts),
    );
  let mk_bounded_pat = selection => {
    let ts = Option.get(Selection.get_tiles(selection));
    (Option.get(Tiles.get_pat(ts)), mk_bounded_ts(Pat, ts));
  };
  let mk_bounded_exp = selection => {
    let ts = Option.get(Selection.get_tiles(selection));
    (Option.get(Tiles.get_exp(ts)), mk_bounded_ts(Exp, ts));
  };
  switch (frame) {
  | Pat(Paren_body((tframe, frame))) =>
    let (body, l_body) = mk_bounded_pat(subject);
    let tframe = TupleUtil.map2(Tiles.of_pat, tframe);
    Some((Pat(Paren(body)), mk_Paren(l_body), tframe, Pat(frame)));
  | Pat(Lam_pat((tframe, frame))) =>
    let (p, l_p) = mk_bounded_pat(subject);
    let tframe = TupleUtil.map2(Tiles.of_exp, tframe);
    Some((Exp(Lam(p)), mk_Lam(l_p), tframe, Exp(frame)));
  | Pat(Let_pat(def, (tframe, frame))) =>
    let (p, l_p) = mk_bounded_pat(subject);
    let l_def = mk_ts(Exp, Tiles.of_exp(def));
    let tframe = TupleUtil.map2(Tiles.of_exp, tframe);
    Some((Exp(Let(p, def)), mk_Let(l_p, l_def), tframe, Exp(frame)));
  | Exp(Paren_body((tframe, frame))) =>
    let (body, l_body) = mk_bounded_exp(subject);
    let tframe = TupleUtil.map2(Tiles.of_exp, tframe);
    Some((Exp(Paren(body)), mk_Paren(l_body), tframe, Exp(frame)));
  | Exp(Let_def(p, (tframe, frame))) =>
    let (def, l_def) = mk_bounded_exp(subject);
    let l_p = mk_ts(Pat, Tiles.of_pat(p));
    let tframe = TupleUtil.map2(Tiles.of_exp, tframe);
    Some((Exp(Let(p, def)), mk_Let(l_p, l_def), tframe, Exp(frame)));
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
        l_root_tile: t,
        (child_pre, child_suf): ListFrame.t(Tile.t),
        (prefix, suffix): ListFrame.t(Tile.t),
      ) => {
    let c = Color.of_sort(sort);
    let uni_child = uni_child(~sort);

    let mk_prefix = (offset, prefix) =>
      prefix
      |> List.rev
      |> List.mapi((i, tile) =>
           [space(offset + i, c), step(offset + i, mk_tile(tile))]
         )
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
        );
      mk_frame(
        mk_subject(~sort=frame_sort, l_root_tile, uni_children, tframe)
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
      mk_root_tile(~offset=List.length(prefix), ~sort=frame_sort, root_tile);
    mk_frame(
      mk_subject(~sort=Frame.sort(frame), l_root_tile, uni_children, tframe),
      frame,
    );
  };
};

let mk_affix =
    (
      ~show_children: bool,
      ~sort: Sort.t,
      ~offset: int,
      d: Direction.t,
      selems,
    ) => {
  let cr = (i, offset) => d == Left ? offset - i : offset + i;
  switch (selems) {
  | [] => empty
  | [_, ..._] =>
    selems
    |> List.mapi((i, selem) => {
         let n = cr(i, offset);
         let l_selem =
           step(
             n,
             mk_selem(
               ~style=
                 Selection.filter_pred(sort, selem)
                   ? Revealed({show_children: show_children}) : Filtered,
               Color.of_sort(Selem.sort(selem)),
               selem,
             ),
           );
         let l_space =
           space(cr(1, n), Color.of_sort(snd(Selem.tip(d, selem))));
         [l_selem, l_space];
       })
    |> List.flatten
    |> (
      switch (d) {
      | Left => List.rev
      | Right => (ls => ls)
      }
    )
    |> cats
  };
};

let mk_selecting =
    (
      selection: Selection.t,
      (prefix, suffix): Selection.frame,
      frame: Frame.t,
    ) => {
  let sort_frame = Frame.sort(frame);
  let mk_affix = mk_affix(~show_children=true, ~sort=sort_frame);

  let offset = List.length(prefix);
  let l_prefix = mk_affix(~offset, Left, prefix);

  let l_selection =
    mk_selection(
      ~offset,
      ~style=Selected,
      Color.of_sort(sort_frame),
      selection,
    );

  let offset = offset + List.length(selection);
  let l_suffix = mk_affix(~offset, Right, suffix);

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

  let offset = List.length(prefix);
  let l_prefix = mk_affix(~offset, Left, prefix);
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
