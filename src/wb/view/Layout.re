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
  | Child({
      step: ChildStep.t,
      sort: (Sort.t, Sort.t),
    })
  | Selem({
      step: Path.selem_step,
      color: Color.t,
      shape: selem_shape,
    });

// | TargetBounds({
//     sort: Sort.t,
//     mode: CaretMode.t,
//     strict_bounds: (bool, bool),
//   });

let empty = Text("");

let annot = (annot, l) => Annot(annot, l);

let cat = (l1, l2) => Cat(l1, l2);
let cats =
  fun
  | [] => empty
  | [l, ...ls] => List.fold_left(cat, l, ls);

let join = (sep: t, ls: list(t)) => ls |> ListUtil.join(sep) |> cats;

let delim = s => annot(Delim, Text(s));
let empty_hole = (color, tip) =>
  annot(EmptyHole(color, tip), Text(Unicode.nbsp));
let open_child = (sort, step) => annot(Child({step, sort: (sort, sort)}));
let closed_child = (sort, step) => annot(Child({step, sort}));

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
let pad = (~offset, ~length, color, l) =>
  cats([space(offset, color), l, space(offset + length, color)]);
let pad_spaces = (~offset=0, color, ls) =>
  switch (ls) {
  | [] => space(offset, color)
  | [_, ..._] =>
    pad(~offset, ~length=List.length(ls), color, spaces(color, ls))
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
  origin: int,
  length: int,
};

let measured_fold' =
    (
      ~text: (measurement, string) => 'acc,
      ~cat: (measurement, 'acc, 'acc) => 'acc,
      // let client cut off recursion
      ~annot: (t => 'acc, measurement, annot, t) => 'acc,
      ~origin=0,
      l: t,
    ) => {
  let rec go = (~origin, l: t) => {
    let m = {origin, length: length(l)};
    switch (l) {
    | Text(s) => text(m, s)
    | Cat(l1, l2) =>
      let mid = origin + length(l1);
      cat(m, go(~origin, l1), go(~origin=mid, l2));
    | Annot(ann, l) => annot(go(~origin), m, ann, l)
    };
  };
  go(~origin, l);
};
let measured_fold = (~annot: (measurement, annot, 'acc) => 'acc, ~origin=0) =>
  measured_fold'(~annot=(k, m, ann, l) => annot(m, ann, k(l)), ~origin);

let find_space =
    (~origin=0, n: Path.caret_step, l: t): (Color.t, measurement) => {
  l
  |> measured_fold'(
       ~origin,
       ~text=(_, _) => [],
       ~cat=_ => (@),
       ~annot=
         (_k, measurement, annot, _l) =>
           switch (annot) {
           | Space(m, color) when m == n => [(color, measurement)]
           | _ => []
           },
     )
  |> ListUtil.hd_opt
  |> OptUtil.get_or_raise(Invalid_argument("Layout.find_space"));
};

let find_range =
    (~origin=0, (l, r): (Path.caret_step, Path.caret_step), layout: t) => {
  let (color_l, l) = find_space(~origin, l, layout);
  let (color_r, r) = find_space(~origin, r, layout);
  let measurement = {origin: l.origin, length: r.origin - l.origin};
  ((color_l, color_r), measurement);
};

let find_selem = (~origin=0, step: Path.selem_step, l: t) =>
  l
  |> measured_fold'(
       ~origin,
       ~text=(_, _) => [],
       ~cat=_ => (@),
       ~annot=
         (_k, measurement, annot, l) =>
           switch (annot) {
           | Selem({step: s, color, shape}) when s == step => [
               (measurement, color, shape, l),
             ]
           | _ => []
           },
     )
  |> ListUtil.hd_opt
  |> OptUtil.get_or_raise(Invalid_argument("Layout.find_selem"));

let selem_children =
  measured_fold'(
    ~text=(_, _) => ([], []),
    ~cat=
      (_, (open1, closed1), (open2, closed2)) =>
        (open1 @ open2, closed1 @ closed2),
    ~annot=
      (_k, {origin, length}, annot, _l) =>
        switch (annot) {
        | Child({step: _, sort: (s_out, s_in)}) =>
          s_out == s_in
            ? ([{origin, length}], []) : ([], [{origin, length}])
        | _ => ([], [])
        },
  );
let selem_holes =
  measured_fold(
    ~text=(_, _) => [],
    ~cat=_ => (@),
    ~annot=
      ({origin, _}, annot, holes) =>
        switch (annot) {
        | EmptyHole(sort, tip) => [(origin, sort, tip), ...holes]
        | _ => holes
        },
  );

let mk_Paren = (sort, body) =>
  cats([
    delim("("),
    open_child(sort, ChildStep.paren_body, body),
    delim(")"),
  ]);

let mk_Lam = p =>
  cats([
    delim(Unicode.lam),
    closed_child((Exp, Pat), ChildStep.lam_pat, p),
    delim("."),
  ]);

let mk_Let = (p, def) => {
  cats([
    delim("let"),
    closed_child((Exp, Pat), ChildStep.let_pat, p),
    delim("="),
    open_child(Exp, ChildStep.let_def, def),
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

let selem_shape = (selem: Selem.t) => {
  let (lshape, _) = Selem.tip(Left, selem);
  let (rshape, _) = Selem.tip(Right, selem);
  let ltails = Selem.tails(Left, selem);
  let rtails = Selem.tails(Right, selem);
  ((lshape, ltails), (rshape, rtails));
};

let rec mk_tiles = (~offset=0, ~rail_color as _=?, ts) =>
  List.mapi(
    (i, tile) => {
      let l_tile = mk_tile(tile);
      annot(
        Selem({
          step: offset + i,
          color: Color.of_sort(Tile.sort(tile)),
          shape: selem_shape(Tile(tile)),
        }),
        l_tile,
      );
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
         mk_Paren(Pat, pad_spaces(Pat, mk_tiles(Tiles.of_pat(body))))
       | BinHole => mk_BinHole(Pat, Concave)
       | Prod => mk_Prod(),
       fun
       | Tile_exp.OpHole => mk_OpHole(Exp, Convex)
       | Num(n) => mk_text(string_of_int(n))
       | Var(x) => mk_text(x)
       | Paren(body) =>
         mk_Paren(Exp, pad_spaces(Exp, mk_tiles(Tiles.of_exp(body))))
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

let mk_selem = (step, selem: Selem.t) => {
  let l = Selem.get(mk_token, mk_tile, selem);
  let shape = selem_shape(selem);
  let color = Color.of_sort(Selem.sort(selem));
  annot(Selem({step, shape, color}), l);
};

let mk_selection = (~offset=0, frame_color: Color.t, selection) =>
  switch (Selection.tip_sorts(selection)) {
  | None => space(offset, frame_color)
  | Some((sort_l, _)) =>
    selection
    |> List.mapi((i, selem) =>
         [
           mk_selem(offset + i, selem),
           space(
             offset + i + 1,
             Color.of_sort(snd(Selem.tip(Right, selem))),
           ),
         ]
       )
    |> List.flatten
    |> List.cons(space(offset, Color.of_sort(sort_l)))
    |> cats
  };

let rec mk_frame = (subject: t, frame: Frame.t): t => {
  let mk_tiles_pat = (~offset=0, ts) =>
    mk_tiles(~offset, List.map(Tile.pat, ts));
  let mk_tiles_exp = (~offset=0, ts) =>
    mk_tiles(~offset, List.map(Tile.exp, ts));
  let mk_frame_pat =
      ((tile, shape), ((prefix, suffix), frame): Frame_pat.s) => {
    let step = List.length(prefix);
    let ls_prefix = mk_tiles_pat(List.rev(prefix));
    let ls_suffix = mk_tiles_pat(~offset=step + 1, suffix);
    let selem_ann = Selem({step, shape, color: Pat});
    mk_frame(
      pad_spaces(Pat, ls_prefix @ [annot(selem_ann, tile), ...ls_suffix]),
      Pat(frame),
    );
  };
  let mk_frame_exp =
      ((tile, shape), ((prefix, suffix), frame): Frame_exp.s) => {
    let step = List.length(prefix);
    let ls_prefix = mk_tiles_exp(List.rev(prefix));
    let ls_suffix = mk_tiles_exp(~offset=step + 1, suffix);
    let selem_ann = Selem({step, shape, color: Exp});
    mk_frame(
      pad_spaces(Exp, ls_prefix @ [annot(selem_ann, tile), ...ls_suffix]),
      Exp(frame),
    );
  };
  let shape_op = Tip.((Convex, 0), (Convex, 0));
  let shape_pre = Tip.((Convex, 0), (Concave, 0));
  switch (frame) {
  | Pat(Paren_body(frame_s)) =>
    let tile = mk_Paren(Pat, subject);
    mk_frame_pat((tile, shape_op), frame_s);
  | Pat(Lam_pat(frame_s)) =>
    let tile = mk_Lam(subject);
    mk_frame_exp((tile, shape_pre), frame_s);
  | Pat(Let_pat(def, frame_s)) =>
    let tile = mk_Let(subject, pad_spaces(Exp, mk_tiles_exp(def)));
    mk_frame_exp((tile, shape_pre), frame_s);
  | Exp(Paren_body(frame_s)) =>
    let tile = mk_Paren(Exp, subject);
    mk_frame_exp((tile, shape_op), frame_s);
  | Exp(Let_def(p, frame_s)) =>
    let tile = mk_Let(pad_spaces(Pat, mk_tiles_pat(p)), subject);
    mk_frame_exp((tile, shape_pre), frame_s);
  | Exp(Root) => subject
  };
};

let mk_pointing = (sframe: Selection.frame, frame: Frame.t) => {
  let color = Color.of_sort(Frame.sort(frame));
  let subject =
    ListFrame.to_list(sframe)
    |> List.mapi((i, selem) => {mk_selem(i, selem)})
    |> pad_spaces(color);
  mk_frame(subject, frame);
};

let mk_selecting =
    (selection: Selection.t, sframe: Selection.frame, frame: Frame.t) => {
  let subject =
    ListFrame.to_list(~subject=selection, sframe)
    |> List.mapi((i, selem) =>
         [
           mk_selem(i, selem),
           space(i + 1, Color.of_sort(snd(Selem.tip(Right, selem)))),
         ]
       )
    |> List.flatten
    |> List.cons(space(0, Color.of_sort(Frame.sort(frame))))
    |> cats;
  mk_frame(subject, frame);
};

let mk_restructuring =
    (_selection: Selection.t, sframe: Selection.frame, frame: Frame.t) => {
  let subject =
    ListFrame.to_list(sframe)
    |> List.mapi((i, selem) =>
         [
           mk_selem(i, selem),
           space(i + 1, Color.of_sort(snd(Selem.tip(Right, selem)))),
         ]
       )
    |> List.flatten
    |> List.cons(space(0, Color.of_sort(Frame.sort(frame))))
    |> cats;
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
