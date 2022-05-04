open Util;
open Core;

// TODO: get rid of these and better organize
// types around new mold/nib model
[@deriving show]
type tip_shape = (Nib.t, int);
[@deriving show]
type piece_shape = (tip_shape, tip_shape);

[@deriving show]
type t =
  | Text(string)
  | Cat(t, t)
  | Annot(annot, t)
and annot =
  | ExtraBoldDelim
  | Delim
  | EmptyHole(Color.t, Nib.t)
  | Ap
  | Space(int, Color.t)
  | Child({
      step: ChildStep.t,
      sort: (Sort.t, Sort.t),
    })
  | Piece({
      step: Path.piece_step,
      color: Color.t,
      shape: piece_shape,
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
let extra_bold_delim = s => annot(ExtraBoldDelim, Text(s));
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
  | [] => empty
  //TODO(andrew): david i made this change to avoid a surperfluous space being inserted, not sure if correct
  //space(offset, color)
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

let find_piece = (~origin=0, step: Path.piece_step, l: t) =>
  l
  |> measured_fold'(
       ~origin,
       ~text=(_, _) => [],
       ~cat=_ => (@),
       ~annot=
         (_k, measurement, annot, l) =>
           switch (annot) {
           | Piece({step: s, color, shape}) when s == step => [
               (measurement, color, shape, l),
             ]
           | _ => []
           },
     )
  |> ListUtil.hd_opt
  |> OptUtil.get_or_raise(Invalid_argument("Layout.find_piece"));

let piece_children =
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
let piece_holes =
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

let paren_l = extra_bold_delim("(");
let paren_r = extra_bold_delim(")");

let ap_l = extra_bold_delim("[");
let ap_r = extra_bold_delim("]");

let lam_lam = extra_bold_delim("\\");
let lam_open = extra_bold_delim("{");
let lam_close = extra_bold_delim("}");

let mk_Paren = (sort, body) =>
  cats([paren_l, open_child(sort, ChildStep.paren_body, body), paren_r]);

let mk_Ap = arg =>
  cats([ap_l, open_child(Exp, ChildStep.ap_arg, arg), ap_r]);

let mk_Lam = (p, body) =>
  cats([
    lam_lam,
    closed_child((Exp, Pat), ChildStep.lam_pat, p),
    lam_open,
    open_child(Exp, ChildStep.lam_body, body),
    lam_close,
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

let mk_Cond = then_ =>
  cats([
    delim("?"),
    open_child(Exp, ChildStep.cond_then, then_),
    delim(":"),
  ]);

let mk_Fact = () => Text("!");

let mk_Plus = () => Text("+");
let mk_Minus = () => Text("-");
let mk_Times = () => Text("*");
let mk_Div = () => Text("/");

let mk_Prod = () => Text(",");

let mk_OpHole = empty_hole;
let mk_BinHole = empty_hole;

let mk_text = s => Text(s);

// let piece_shape = (piece: Piece.t) => {
//   let (lshape, _) = Piece.tip(Left, piece);
//   let (rshape, _) = Piece.tip(Right, piece);
//   let ltails = Piece.tails(Left, piece);
//   let rtails = Piece.tails(Right, piece);
//   ((lshape, ltails), (rshape, rtails));
// };

// let rec mk_tiles = (~offset=0, ~rail_color as _=?, ts) =>
//   List.mapi(
//     (i, tile) => {
//       let l_tile = mk_tile(tile);
//       annot(
//         Piece({
//           step: offset + i,
//           color: Color.of_sort(Tile.sort(tile)),
//           shape: piece_shape(Tile(tile)),
//         }),
//         l_tile,
//       );
//     },
//     ts,
//   )
// and mk_tile = t =>
//   t
//   |> Tile.get(
//        fun
//        | Tile_pat.OpHole => mk_OpHole(Pat, Convex)
//        | Var(x) => mk_text(x)
//        | Paren(body) =>
//          // TODO undo unnecessary rewrapping
//          mk_Paren(Pat, pad_spaces(Pat, mk_tiles(Tiles.of_pat(body))))
//        | BinHole => mk_BinHole(Pat, Concave)
//        | Prod => mk_Prod(),
//        fun
//        | Tile_exp.OpHole => mk_OpHole(Exp, Convex)
//        | Num(n) => mk_text(string_of_int(n))
//        | Var(x) => mk_text(x)
//        | Paren(body) =>
//          mk_Paren(Exp, pad_spaces(Exp, mk_tiles(Tiles.of_exp(body))))
//        | Ap(arg) => mk_Ap(pad_spaces(Exp, mk_tiles(Tiles.of_exp(arg))))
//        | Lam(p, body) =>
//          mk_Lam(
//            pad_spaces(Pat, mk_tiles(Tiles.of_pat(p))),
//            pad_spaces(Exp, mk_tiles(Tiles.of_exp(body))),
//          )
//        | Let(p, def) =>
//          mk_Let(
//            pad_spaces(Pat, mk_tiles(Tiles.of_pat(p))),
//            pad_spaces(Exp, mk_tiles(Tiles.of_exp(def))),
//          )
//        | Fact => mk_Fact()
//        | BinHole => mk_BinHole(Exp, Concave)
//        | Plus => mk_Plus()
//        | Minus => mk_Minus()
//        | Times => mk_Times()
//        | Div => mk_Div()
//        | Prod => mk_Prod()
//        | Cond(then_) =>
//          mk_Cond(pad_spaces(Exp, mk_tiles(Tiles.of_exp(then_)))),
//      );

// let mk_token =
//   Shard.get(
//     fun
//     | Shard_pat.Paren_l => paren_l
//     | Paren_r => paren_r,
//     fun
//     | Shard_exp.Paren_l => paren_l
//     | Paren_r => paren_r
//     | Ap_l => ap_l
//     | Ap_r => ap_r
//     | Lam_lam => lam_lam
//     | Lam_open => lam_open
//     | Lam_lam_open(p) =>
//       cats([
//         lam_lam,
//         closed_child(
//           (Exp, Pat),
//           ChildStep.lam_pat,
//           pad_spaces(Pat, mk_tiles(Tiles.of_pat(p))),
//         ),
//         lam_open,
//       ])
//     | Lam_close => lam_close
//     | Let_let => delim("let")
//     | Let_eq => delim("=")
//     | Let_let_eq(p) =>
//       cats([
//         delim("let"),
//         closed_child(
//           (Exp, Pat),
//           ChildStep.let_pat,
//           pad_spaces(Pat, mk_tiles(Tiles.of_pat(p))),
//         ),
//         delim("="),
//       ])
//     | Let_in => delim("in")
//     | Cond_que => delim("?")
//     | Cond_col => delim(":"),
//   );

// let mk_piece = (step, piece: Piece.t) => {
//   let l = Piece.get(mk_token, mk_tile, piece);
//   let shape = piece_shape(piece);
//   let color = Color.of_sort(Piece.sort(piece));
//   annot(Piece({step, shape, color}), l);
// };

// let mk_selection = (~offset=0, ~frame_color: Color.t, selection) =>
//   switch (Selection.tip_sorts(selection)) {
//   | None => space(offset, frame_color)
//   | Some((sort_l, _)) =>
//     selection
//     |> List.mapi((i, piece) =>
//          [
//            mk_piece(offset + i, piece),
//            space(
//              offset + i + 1,
//              Color.of_sort(snd(Piece.tip(Right, piece))),
//            ),
//          ]
//        )
//     |> List.flatten
//     |> List.cons(space(offset, Color.of_sort(sort_l)))
//     |> cats
//   };

// let mk_relem = (~step, ~frame_color, relem: Restructuring.frame_elem) =>
//   switch (relem) {
//   | Tile(tile) => mk_piece(step, Tile(tile))
//   | Selection(selection) =>
//     mk_selection(~offset=step, ~frame_color, selection)
//   };

// let rec mk_frame = (subject: t, frame: Frame.t): t => {
//   let mk_tiles_pat = (~offset=0, ts) =>
//     mk_tiles(~offset, List.map(Tile.pat, ts));
//   let mk_tiles_exp = (~offset=0, ts) =>
//     mk_tiles(~offset, List.map(Tile.exp, ts));
//   let mk_frame_pat =
//       ((tile, shape), ((prefix, suffix), frame): Frame_pat.s) => {
//     let step = List.length(prefix);
//     let ls_prefix = mk_tiles_pat(List.rev(prefix));
//     let ls_suffix = mk_tiles_pat(~offset=step + 1, suffix);
//     let piece_ann = Piece({step, shape, color: Pat});
//     mk_frame(
//       pad_spaces(Pat, ls_prefix @ [annot(piece_ann, tile), ...ls_suffix]),
//       Pat(frame),
//     );
//   };
//   let mk_frame_exp =
//       ((tile, shape), ((prefix, suffix), frame): Frame_exp.s) => {
//     let step = List.length(prefix);
//     let ls_prefix = mk_tiles_exp(List.rev(prefix));
//     let ls_suffix = mk_tiles_exp(~offset=step + 1, suffix);
//     let piece_ann = Piece({step, shape, color: Exp});
//     mk_frame(
//       pad_spaces(Exp, ls_prefix @ [annot(piece_ann, tile), ...ls_suffix]),
//       Exp(frame),
//     );
//   };
//   let shape_op = Nib.((Convex, 0), (Convex, 0));
//   let shape_pre = Nib.((Convex, 0), (Concave, 0));
//   let shape_post = Nib.((Concave, 0), (Convex, 0));
//   let shape_bin = Nib.((Concave, 0), (Concave, 0));
//   switch (frame) {
//   | Pat(Paren_body(frame_s)) =>
//     let tile = mk_Paren(Pat, subject);
//     mk_frame_pat((tile, shape_op), frame_s);
//   | Pat(Lam_pat(body, frame_s)) =>
//     let tile = mk_Lam(subject, pad_spaces(Exp, mk_tiles_exp(body)));
//     mk_frame_exp((tile, shape_op), frame_s);
//   | Pat(Let_pat(def, frame_s)) =>
//     let tile = mk_Let(subject, pad_spaces(Exp, mk_tiles_exp(def)));
//     mk_frame_exp((tile, shape_pre), frame_s);
//   | Exp(Paren_body(frame_s)) =>
//     let tile = mk_Paren(Exp, subject);
//     mk_frame_exp((tile, shape_op), frame_s);
//   | Exp(Ap_arg(frame_s)) =>
//     let tile = mk_Ap(subject);
//     mk_frame_exp((tile, shape_post), frame_s);
//   | Exp(Lam_body(p, frame_s)) =>
//     let tile = mk_Lam(pad_spaces(Pat, mk_tiles_pat(p)), subject);
//     mk_frame_exp((tile, shape_op), frame_s);
//   | Exp(Let_def(p, frame_s)) =>
//     let tile = mk_Let(pad_spaces(Pat, mk_tiles_pat(p)), subject);
//     mk_frame_exp((tile, shape_pre), frame_s);
//   | Exp(Cond_then(frame_s)) =>
//     let tile = mk_Cond(subject);
//     mk_frame_exp((tile, shape_bin), frame_s);
//   | Exp(Root) => subject
//   };
// };

// let mk_pointing = (sframe: Selection.frame, frame: Frame.t) => {
//   let color = Color.of_sort(Frame.sort(frame));
//   let subject =
//     ListFrame.to_list(sframe)
//     |> List.mapi((i, piece) => {mk_piece(i, piece)})
//     |> pad_spaces(color);
//   mk_frame(subject, frame);
// };

// let mk_selecting =
//     (selection: Selection.t, sframe: Selection.frame, frame: Frame.t) => {
//   let subject =
//     ListFrame.to_list(~subject=selection, sframe)
//     |> List.mapi((i, piece) =>
//          [
//            mk_piece(i, piece),
//            space(i + 1, Color.of_sort(snd(Piece.tip(Right, piece)))),
//          ]
//        )
//     |> List.flatten
//     |> List.cons(space(0, Color.of_sort(Frame.sort(frame))))
//     |> cats;
//   mk_frame(subject, frame);
// };

// let mk_restructuring =
//     (
//       _backpack: Restructuring.Backpack.t,
//       rframe: Restructuring.frame,
//       frame: Frame.t,
//     ) => {
//   let frame_sort = Frame.sort(frame);
//   let subject =
//     Restructuring.get_sframe(rframe)
//     |> ListFrame.to_list
//     |> List.mapi((i, piece) =>
//          [
//            mk_piece(i, piece),
//            space(i + 1, Color.of_sort(snd(Piece.tip(Right, piece)))),
//          ]
//        )
//     |> List.flatten
//     |> List.cons(space(0, Color.of_sort(frame_sort)))
//     |> cats;
//   mk_frame(subject, frame);
// };

// let mk_zipper =
//   Memo.memoize((zipper: Zipper.t) =>
//     switch (zipper) {
//     | (Pointing(sframe), frame) => mk_pointing(sframe, frame)
//     | (Selecting(_, selection, sframe), frame) =>
//       mk_selecting(selection, sframe, frame)
//     | (Restructuring((backpack, rframe)), frame) =>
//       mk_restructuring(backpack, rframe, frame)
//     }
//   );

// let mk_subject = ((down, up): Subject.t) => {
//   ListFrame.to_list(~subject=selection, sframe)
//   |> List.mapi((i, piece) =>
//        [
//          mk_piece(i, piece),
//          space(i + 1, Color.of_sort(snd(Piece.tip(Right, piece)))),
//        ]
//      )
//   |> List.flatten
//   |> List.cons(space(0, Color.of_sort(Frame.sort(frame))))
//   |> cats;
// };

// let mk_zipper =
//   Memo.memoize(((subj, frame): Zipper.t) =>
//     mk_frame(mk_subject(subj), frame)
//   );

let text: string => t = t => Text(t);

let of_grout: Grout.t => t =
  fun
  | Convex => Text("TODO:CONVEX_GROUT")
  | Concave => Text("TODO:CONCAVE_GROUT");

let of_shard: Base.Shard.t => t =
  ({label: (idx, label), _}) => Text(List.nth(label, idx));

let map_alt: (list('x), list('y), 'x => t, 'y => t) => list(t) =
  (xs, ys, fx, fy) => {
    assert(List.length(xs) == List.length(ys) + 1);
    List.fold_left2(
      (acc, x, y) => acc @ [fy(y), fx(x)],
      [fx(List.hd(xs))],
      List.tl(xs),
      ys,
    );
  };

let rec of_piece: Piece.t => t =
  fun
  | Tile(t) => of_tile(t)
  | Grout(g) => of_grout(g)
  | Shard(s) => of_shard(s)
and of_segment: Segment.t => t =
  seg => seg |> List.map(of_piece) |> pad_spaces(Exp) //TODO: pad?
and of_tile: Tile.t => t =
  ({label, children, _}) =>
    cats(map_alt(label, children, text, of_segment));

let mk_parent: (Ancestor.t, t) => t =
  ({label, children: (left, right), _}, layout) => {
    //TODO(andrew): david does this assert and label splitting logic make sense?
    assert(
      List.length(label) - 2 == List.length(left) + List.length(right),
    );
    let (label_left, label_right) =
      ListUtil.split_n(List.length(left) + 1, label);
    cats(
      map_alt(label_left, left, text, of_segment)
      @ [layout]
      @ map_alt(label_right, right, text, of_segment),
    );
  };

let mk_ancestor: ((Ancestor.t, Siblings.t), t) => t =
  ((ancestor, (left_aunts, right_aunts)), layout) =>
    pad_spaces(
      Exp,
      List.map(of_piece, left_aunts)
      @ [mk_parent(ancestor, layout)]
      @ List.map(of_piece, right_aunts),
    );

let mk_zipper: Zipper.t => t =
  ({relatives: {siblings: (left, right), ancestors, _}, _}) =>
    List.fold_left(
      (layout, ancestor) => mk_ancestor(ancestor, layout),
      cat(of_segment(left), of_segment(right)),
      ancestors,
    );
