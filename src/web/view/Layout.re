open Util;
open Core;

// TODO: get rid of these and better organize
// types around new mold/nib model
[@deriving show]
type tip_shape = (Nib.t, int);
[@deriving show]
type piece_shape = (tip_shape, tip_shape);

[@deriving show]
type ann_cat =
  | None
  | SelectionRange(int, int)
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

[@deriving show]
type ann_text =
  | None
  | Delim
  | DelimBold
  | EmptyHole(Nib.t)
  | Space(int, Color.t)
  | Ap; //TODO(andrew): deprecate?

[@deriving show]
type t =
  | Text(string, ann_text)
  | Cat(list(t), ann_cat);

[@deriving show]
type measurement = {
  origin: int,
  length: int,
};

[@deriving show]
type layoutM =
  | TextM(string, ann_text)
  | CatM(list(measured), ann_cat)
and measured = {
  measurement,
  layout: layoutM,
};

let cat: list(t) => t = xs => Cat(xs, None);
let text: string => t = t => Text(t, None);
let delim: string => t = s => Text(s, Delim);
let space = (n, color) => Text(Unicode.nbsp, Space(n, color));
let empty_hole = tip => Text(Unicode.nbsp, EmptyHole(tip));

let color: Mold.t => Color.t = m => Color.of_sort(m.sorts.out);

let update_ann: (t, ann_cat => ann_cat) => t =
  (t, f) =>
    switch (t) {
    | Cat(x, ann) => Cat(x, f(ann))
    | _ => t
    };

let rec length =
  fun
  | Text(s, _) => Unicode.length(s)
  | Cat(ls, _) => List.fold_left((acc, l) => length(l) + acc, 0, ls);

let rec to_measured = (~origin=0, layout: t): measured =>
  switch (layout) {
  | Text(s, ann) =>
    let measurement = {origin, length: Unicode.length(s)};
    {layout: TextM(s, ann), measurement};
  | Cat(ls, ann) =>
    let (ms, final) =
      List.fold_left(
        ((ms, origin), l) => {
          let m = to_measured(l, ~origin);
          ([m, ...ms], origin + m.measurement.length);
        },
        ([], origin),
        ls,
      );
    let measurement = {origin, length: final - origin};
    {layout: CatM(List.rev(ms), ann), measurement};
  };

let pad_spaces: (Color.t, list(t)) => t =
  (color, ls) =>
    switch (ls) {
    | [] => cat([])
    | _ =>
      let spaces = List.init(List.length(ls) + 1, i => space(i, color));
      cat(ListUtil.interleave(spaces, ls));
    };

let delims =
  List.flatten([
    ["(", ")"],
    ["λ", "{", "}"],
    ["[", "]"],
    [","],
    ["?", ":"],
    ["let", "=", "in"],
  ]);

let text': Token.t => t = t => List.mem(t, delims) ? delim(t) : text(t);

let of_grout: Grout.t => t =
  // TODO(andrew): get nibs for holes?
  fun
  | Convex => empty_hole({shape: Convex, sort: Exp})
  | Concave => empty_hole({shape: Concave(Precedence.max), sort: Exp});

let of_shard: Base.Shard.t => t =
  ({label: (n, label), _}) => text(List.nth(label, n));

let rec of_piece: Piece.t => t =
  fun
  | Tile(t) => of_tile(t)
  | Grout(g) => of_grout(g)
  | Shard(s) => of_shard(s)
and of_pieces = ps => List.map(of_piece, ps)
and of_segment: (Color.t, Segment.t) => t =
  //TODO(andrew): piece step annos
  (color, ps) => ps |> of_pieces |> pad_spaces(color)
and of_form: (Mold.t, list(Token.t), list(Segment.t)) => list(t) =
  //TODO(andrew): child-step anno
  mold => ListUtil.map_alt(text', of_segment(color(mold)))
and of_tile: Tile.t => t =
  ({label, children, mold}) => cat(of_form(mold, label, children));

let of_ancestor: (Ancestor.t, t) => t =
  ({label, children: (l_kids, r_kids), mold}, layout) => {
    assert(
      List.length(label) - 2 == List.length(l_kids) + List.length(r_kids),
    );
    let (l_label, r_label) =
      ListUtil.split_n(List.length(l_kids) + 1, label);
    cat(
      of_form(mold, l_label, List.rev(l_kids))
      @ [layout]
      @ of_form(mold, r_label, r_kids),
    );
  };

let of_generation: (t, (Ancestor.t, Siblings.t)) => t =
  (layout, (ancestor, (l_pibs, r_pibs))) => {
    pad_spaces(
      color(ancestor.mold),
      //TODO(andrew): piece-step annos
      of_pieces(List.rev(l_pibs))
      @ [of_ancestor(ancestor, layout)]
      @ of_pieces(r_pibs),
    );
  };

let ann_selection: (t, (list('a), list('b))) => t =
  (layout, (l_sibs, content)) => {
    let l = List.length(l_sibs);
    let r = l + List.length(content);
    //NOTE: increment indicies because of space padding
    update_ann(layout, _ => SelectionRange(l + 1, r + 1));
  };

let mk_zipper: Zipper.t => t =
  (
    {
      relatives: {siblings: (l_sibs, r_sibs), ancestors},
      selection: {content, _},
      _,
    },
  ) => {
    let color = ancestors |> Ancestors.sort |> Color.of_sort;
    let segments = List.rev(l_sibs) @ content @ r_sibs;
    let layout = of_segment(color, segments);
    let current = ann_selection(layout, (l_sibs, content));
    List.fold_left(of_generation, current, ancestors);
  };

// OLD

let measured_fold' =
    (
      ~text: (measurement, string) => 'acc,
      ~cat: (measurement, list('acc)) => 'acc,
      // let client cut off recursion
      ~annot as _: (t => 'acc, measurement, ann_cat, t) => 'acc,
      ~origin=0,
      l: t,
    ) => {
  let rec go = (~origin, l: t) => {
    let m = {origin, length: length(l)};
    switch (l) {
    | Text(s, _) => text(m, s)
    | Cat(ls, _) =>
      let (acc, _) =
        List.fold_left(
          ((acc, origin), l) => {
            (acc @ [go(~origin, l)], origin + length(l))
          },
          ([], origin),
          ls,
        );
      cat(m, acc);
    };
  };
  go(~origin, l);
};
let measured_fold = (~annot: (measurement, ann_cat, 'acc) => 'acc, ~origin=0) =>
  measured_fold'(~annot=(k, m, ann, l) => annot(m, ann, k(l)), ~origin);

let find_space =
    (~origin=0, n: Path.caret_step, l: t): (Color.t, measurement) => {
  l
  |> measured_fold'(
       ~origin,
       ~text=(_, _) => [],
       ~cat=_ => List.concat,
       ~annot=
         (_k, measurement, _annot, l) =>
           switch (l) {
           | Text(_, Space(m, color)) when m == n => [(color, measurement)]
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
       ~cat=_ => List.concat,
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
      (_, xs) =>
        List.fold_left(
          ((acc_l, acc_r), (x_l, x_r)) => (acc_l @ x_l, acc_r @ x_r),
          ([], []),
          xs,
        ),
    /*(_, (open1, closed1), (open2, closed2)) =>
      (open1 @ open2, closed1 @ closed2),*/
    ~annot=
      (_k, {origin, length}, annot, _l) =>
        switch (annot) {
        | Child({step: _, sort: (s_out, s_in)}) =>
          s_out == s_in
            ? ([{origin, length}], []) : ([], [{origin, length}])
        | _ => ([], [])
        },
  ) /* }*/;
//let piece_holes =
//  measured_fold(
//    ~text=(_, _) => [],
//    ~cat=_ => List.concat,
//    ~annot=
//      ({origin, _}, annot, holes) =>
//        switch (annot) {
//        | EmptyHole(sort, tip) => [(origin, sort, tip), ...holes]
//        | _ => holes
//        },
//  );

//let paren_l = extra_bold_delim("(");
//let paren_r = extra_bold_delim(")");

//let ap_l = extra_bold_delim("[");
//let ap_r = extra_bold_delim("]");

//let lam_lam = extra_bold_delim("\\");
//let lam_open = extra_bold_delim("{");
//let lam_close = extra_bold_delim("}");

//let mk_Paren = (sort, body) =>
//  cat([paren_l, open_child(sort, ChildStep.paren_body, body), paren_r]);

//let mk_Ap = arg => cat([ap_l, open_child(Exp, ChildStep.ap_arg, arg), ap_r]);

//let mk_Lam = (p, body) =>
//  cat([
//    lam_lam,
//    closed_child((Exp, Pat), ChildStep.lam_pat, p),
//    lam_open,
//    open_child(Exp, ChildStep.lam_body, body),
//    lam_close,
//  ]);

//let mk_Let = (p, def) => {
//  cat([
//    delim("let"),
//    closed_child((Exp, Pat), ChildStep.let_pat, p),
//    delim("="),
//    open_child(Exp, ChildStep.let_def, def),
//    delim("in"),
//  ]);
//};

//let mk_Cond = then_ =>
//  cat([
//    delim("?"),
//    open_child(Exp, ChildStep.cond_then, then_),
//    delim(":"),
//  ]);

//let mk_Fact = () => Text("!");
//let mk_Plus = () => Text("+");
//let mk_Minus = () => Text("-");
//let mk_Times = () => Text("*");
//let mk_Div = () => Text("/");
//let mk_Prod = () => Text(",");
//let mk_OpHole = empty_hole;
//let mk_BinHole = empty_hole;

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
