// open Util;
open Core;

let pad_segments = false; // set segments as space-padded

// open Virtual_dom;
// open Vdom;
// let span_c = cls => Node.span([Attr.class_(cls)]);
// let rec text_of_segment = (seg: Segment.t): list(Node.t) =>
//   seg
//   |> List.map(
//        fun
//        | Piece.Whitespace(w) => [Node.text(w.content)]
//        | Grout(_) => [Node.text(Unicode.nbsp)]
//        | Tile(t) => text_of_tile(t),
//      )
//   |> List.concat
// and text_of_tile = (t: Tile.t): list(Node.t) => {
//   let cls = Tile.is_complete(t) ? "delim" : "extra-bold-delim";
//   Aba.mk(t.shards, t.children)
//   |> Aba.join(
//        i => [span_c(cls, [Node.text(List.nth(t.label, i))])],
//        text_of_segment,
//      )
//   |> List.concat;
// };
// let text_of_zipper = z => text_of_segment(Zipper.zip(z));
// let pad_segments = true; // set segments as space-padded
// // TODO: get rid of these and better organize
// // types around new mold/nib model
[@deriving show]
type tip_shape = (Nib.t, int);
[@deriving show]
type piece_shape = (tip_shape, tip_shape);
let piece_shape_of_nibs = ((l, r): Nibs.t): piece_shape => (
  (l, 0),
  (r, 0),
);
[@deriving show]
type measurement = {
  origin: int,
  length: int,
};
[@deriving show]
type padding =
  | None
  | Bi
  | Pre
  | Post;

[@deriving show]
type token = {
  string,
  padding,
};
[@deriving show]
type selection_focus =
  /* The selection is a range in the focal segment. The left and right
      extremes of the selection correspond to spaces between the pieces
      of the segment. An empty selecton is equivalent/coincident to/with
      the caret. If there is a piece directly to the right of an empty
      selection, that pience is Indicated. If the empty selection is at the
     last space in the focal segment, the containing piece is Indicated */
  //| Indicated
  /* A Selected piece contained inside the selection, unless it is... */
  | Selected
  /* TODO: A shard inside the selction which has partner(s) on the outside.
     Are there actually any cases of shards outside the backpack which aren't
     either SelectedPartner/PartnerSelected? If not we don't need these
     and could instead just store whether a piece is a shard */
  //| SelectedPartner
  /* TODO: A shard outside the selection which has partner(s) inside */
  //| PartnerSelected
  /* A non-shard piece which is outside the selection and not indicated */
  | NotSelected;

[@deriving show]
type piece_focus =
  | Indicated(token)
  | OutsideFocalSegment
  | InsideFocalSegment(selection_focus);
[@deriving show]
type segment_focus =
  | None // NOTE: Could add (eg) Above, Below, SiblingOf cursor
  | Range(int, int); // Marks the focal segment, which contains the selection
[@deriving show]
type piece =
  | Tile
  | Shard
  | Grout;
[@deriving show]
type ann_cat =
  | Piece(piece, Mold.t, piece_focus)
  | Segment(segment_focus);
[@deriving show]
type ann_atom =
  | None
  | Delim
  | Shard
  | EmptyHole(Mold.t)
  | Space(int, Color.t)
  | Whitespace
  | Ap; //TODO(andrew): deprecate?
[@deriving show]
type t =
  | Atom(token, ann_atom)
  | Cat(list(t), ann_cat);

[@deriving show]
type layoutM =
  | AtomM(token, ann_atom)
  | CatM(list(measured), ann_cat)
and measured = {
  measurement,
  layout: layoutM,
};

/*let get_shape: layoutM => option(Mold.Shape.t) =
  fun
  | CatM(_, Piece(_, {shape, _}, _)) => Some(shape)
  | _ => None;*/

let padding: string => padding =
  fun
  | "fun"
  | "let" => Post
  | "=>"
  | "+"
  | "-"
  | "*"
  | "/"
  | ","
  | "="
  | "in"
  | "?"
  | ":" => Bi
  | "("
  | ")"
  | "["
  | "]"
  | "}"
  | _ => None;

let string_of_token = ({string, padding}: token) =>
  switch (padding) {
  | None => string
  | Pre => " " ++ string
  | Post => string ++ " "
  | Bi => " " ++ string ++ " "
  };

let token_length: token => int = t => t |> string_of_token |> Unicode.length;

let unpadded = (string): token => {string, padding: None};

let delim_token: Token.t => token =
  string => {string, padding: pad_segments ? None : padding(string)};

// let text: string => t = t => Atom(t, None);
// let delim: string => t = s => Atom(s, Delim);
// let text': Token.t => t = t => List.mem(t, delims) ? delim(t) : text(t);
// let shard: string => t = s => Atom(s, Shard);
// let placeholder = ann => Atom(Unicode.nbsp, ann);
// let space = (n, sort) => placeholder(Space(n, Color.of_sort(sort)));

let cat_piece: (piece, Mold.t, list(t)) => t =
  (p, m, ls) => Cat(ls, Piece(p, m, OutsideFocalSegment));

/*
 let grout_token = (g: Grout.t): token => {
   let padding: padding =
     switch (g) {
     | _ when pad_segments => None
     | (Concave(_), Concave(_)) => Bi
     | _ => None
     };
   // alternate string forms: "⬣" "⧗"
   {string: Unicode.nbsp, padding};
 };

 let shard_token = (n, label): token => {
   assert(n >= 0 && n < List.length(label));
   let string = List.nth(label, n);
   delim_token(string);
 };

 let token_of_deprecate = (idx: int, p: Piece.t): token =>
   switch (p) {
   | Tile({label, _}) => delim_token(List.nth(label, idx))
   | Grout(g) => grout_token(g)
   | Shard({label: (n, lb), _}) => shard_token(n, lb)
   };


 let cat_segment: (Sort.t, list(t)) => t =
   (sort, ls) =>
     switch (ls) {
     | [] => Cat([], Segment(None))
     | _ =>
       let space = (n, sort) =>
         Atom(unpadded(Unicode.nbsp), Space(n, Color.of_sort(sort)));
       let spaces = List.init(List.length(ls) + 1, i => space(i, sort));
       let ls = pad_segments ? ListUtil.interleave(spaces, ls) : ls;
       Cat(ls, Segment(None));
     };
  */

let update_ann: (t, ann_cat => ann_cat) => t =
  (t, f) =>
    switch (t) {
    | Cat(x, ann) => Cat(x, f(ann))
    | _ => t
    };

let set_piece_focus = (focus: piece_focus, l: t): t =>
  update_ann(l, x =>
    switch (x) {
    | Piece(p, m, _) => Piece(p, m, focus)
    | _ => x
    }
  );

// [@deriving show]
// type layoutM =
//   | AtomM(string, ann_atom)
//   | CatM(list(measured), ann_cat)
// and measured = {
//   measurement,
//   layout: layoutM,
// };
// let text: string => t = t => Atom(t, None);
// let delim: string => t = s => Atom(s, Delim);
// let text': Token.t => t = t => Token.is_delim(t) ? delim(t) : text(t);
// let shard: string => t = s => Atom(s, Shard);
// let ws = s => Atom(s, Whitespace);
// let placeholder = ann => Atom(Unicode.nbsp, ann);
// let space = (n, sort) => placeholder(Space(n, Color.of_sort(sort)));
// let cat_piece: (piece, Mold.t, list(t)) => t =
//   (p, m, ls) => Cat(ls, Piece(p, m, OutsideFocalSegment));
// let cat_segment: (Sort.t, list(t)) => t =
//   (sort, ls) =>
//     switch (ls) {
//     | [] => Cat([], Segment(None))
//     | _ =>
//       let spaces = List.init(List.length(ls) + 1, i => space(i, sort));
//       let ls = pad_segments ? ListUtil.interleave(spaces, ls) : ls;
//       Cat(ls, Segment(None));
//     };
// let update_ann: (t, ann_cat => ann_cat) => t =
//   (t, f) =>
//     switch (t) {
//     | Cat(x, ann) => Cat(x, f(ann))
//     | _ => t
//     };
// let set_piece_focus = (focus: piece_focus, l: t): t =>
//   update_ann(l, x =>
//     switch (x) {
//     | Piece(p, m, _) => Piece(p, m, focus)
//     | _ => x
//     }
//   );
let rec length =
  fun
  | Atom(t, _) => token_length(t)
  | Cat(ls, _) => List.fold_left((acc, l) => length(l) + acc, 0, ls);

let relativize_measurements: (int, list(measurement)) => list(measurement) =
  parent_origin =>
    List.map(({origin, length}) =>
      {origin: origin - parent_origin, length}
    );

/*
 let rec to_measured = (~origin=0, layout: t): measured =>
   switch (layout) {
   | Atom(t, ann) =>
     let measurement = {origin, length: token_length(t)};
     {layout: AtomM(t, ann), measurement};
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

 let segment_idx: int => int = x => pad_segments ? 2 * x + 1 : x;

 let select_piece_idxs = xs =>
   List.map(idx => {
     // NOTE: This re-indexing is because of delims, NOT padding
     let i = 2 * idx + 1;
     assert(i >= 0 && i < List.length(xs));
     List.nth(xs, i);
   });

 let get_closed_children = (mold: Mold.t, ms: list(measured)): list(measured) => {
   List.map((==)(mold.sorts.out), mold.sorts.in_)
   |> ListUtil.p_indices((==)(false))
   |> select_piece_idxs(ms);
 };

 let get_open_children = (mold: Mold.t, ms: list(measured)): list(measured) => {
   List.map((==)(mold.sorts.out), mold.sorts.in_)
   |> ListUtil.p_indices((==)(true))
   |> select_piece_idxs(ms);
 };


 let of_grout: (Sort.t, Grout.t) => t =
   (sort, g) => {
     let token = grout_token(g);
     let mold = Mold.of_grout(g, sort);
     cat_piece(Grout, mold, [Atom(token, EmptyHole(mold))]);
   };

 let of_shard: Base.Shard.t => t =
   ({nibs, label: (n, label), _}) => {
     assert(n >= 0 && n < List.length(label));
     let token = shard_token(n, label);
     let mold = Mold.of_shard(nibs, n, label);
     cat_piece(Shard, mold, [Atom(token, Shard)]);
   };

 let of_tile_token: (Base.Tile.Label.t, Token.t) => t =
   (_lb, s) => Atom(delim_token(s), Token.is_delim(s) ? Delim : None);

 let rec of_piece: (Sort.t, piece_focus, Piece.t) => t =
   (sort, focus, p) => {
     let t =
       switch (p) {
       | Tile(t) => of_tile(t)
       | Grout(g) => of_grout(sort, g)
       | Shard(s) => of_shard(s)
       };
     set_piece_focus(focus, t);
   }
 and of_pieces = sort => List.map(of_piece(sort, OutsideFocalSegment))
 and of_segment: (Sort.t, Segment.t) => t =
   (sort, ps) => ps |> of_pieces(sort) |> cat_segment(sort)
 and of_form: (Mold.t, list(Token.t), list(Segment.t)) => list(t) =
   (mold, label) =>
     ListUtil.map_alt(
       of_tile_token(label),
       of_segment(mold.sorts.out),
       label,
     )
 and of_tile: Tile.t => t =
   ({id: _, label, children, mold}) =>
     cat_piece(Tile, mold, of_form(mold, label, children));

 let indicate_ancestor = lb =>
   set_piece_focus(
     Indicated(lb == [] ? unpadded("") : delim_token(List.hd(lb))),
   );

 let of_ancestor: (~indicate: bool, Ancestor.t, t) => t =
   (~indicate, {id: _, label, children: (l_kids, r_kids), mold}, layout) => {
     assert(List.length(label) == 2 + List.length(l_kids @ r_kids));
     let (lb_l, lb_r) = ListUtil.split_n(1 + List.length(l_kids), label);
     cat_piece(
       Tile,
       mold,
       of_form(mold, lb_l, List.rev(l_kids))
       @ [layout]
       @ of_form(mold, lb_r, r_kids),
     )
     |> (indicate ? indicate_ancestor(lb_r) : (p => p));
   };

 let of_generation: (~indicate: bool, t, Ancestors.generation) => t =
   (~indicate, layout, (ancestor, (l_pibs, r_pibs))) => {
     let sort = ancestor.mold.sorts.out;
     cat_segment(
       sort,
       of_pieces(sort, List.rev(l_pibs))
       @ [of_ancestor(~indicate, ancestor, layout)]
       @ of_pieces(sort, r_pibs),
     );
   };

 let ann_selection: (t, (list('a), list('b))) => t =
   (layout, (l_sibs, content)) => {
     let l = List.length(l_sibs);
     let r = l + List.length(content);
     update_ann(layout, ann =>
       switch (ann) {
       | Piece(_) => ann
       //NOTE: increment indicies because of space padding
       | Segment(_) => Segment(Range(segment_idx(l), segment_idx(r)))
       }
     );
   };

 let mk_zipper: Zipper.t => t =
   (
     {
       relatives: {siblings: (l_sibs, r_sibs), ancestors},
       selection: {content, _},
       _,
     },
   ) => {
     let sort = Ancestors.sort(ancestors);
     let select_piece = of_piece(sort, InsideFocalSegment(Selected));
     let indicate_piece = p =>
       of_piece(sort, Indicated(token_of_deprecate(0, p)), p);
     let snub_piece = of_piece(sort, InsideFocalSegment(NotSelected));
     let selection_ls = content |> List.map(select_piece);
     let l_sibs_ls = List.map(snub_piece, List.rev(l_sibs));
     let r_sibs_ls =
       switch (content, r_sibs) {
       | (_, []) => []
       | ([], [p, ...ps]) =>
         List.cons(indicate_piece(p), List.map(snub_piece, ps))
       | _ => List.map(snub_piece, r_sibs)
       };
     let ls = l_sibs_ls @ selection_ls @ r_sibs_ls;
     let layout = cat_segment(sort, ls);
     let current = ann_selection(layout, (l_sibs, content));
     switch (r_sibs, ancestors, content) {
     | ([], [], []) =>
       // TODO(andrew): cleanup. end of program case
       let ls =
         switch (l_sibs) {
         | [] => []
         | [p, ...ps] =>
           List.rev(List.cons(indicate_piece(p), List.map(snub_piece, ps)))
         };
       let layout = cat_segment(sort, ls);
       let current = ann_selection(layout, (l_sibs, content));
       current;
     | ([], [x, ...xs], _) =>
       // NOTE: if there are no pieces to the right, indicate parent
       let previous = of_generation(~indicate=content == [], current, x);
       List.fold_left(of_generation(~indicate=false), previous, xs);
     | _ => List.fold_left(of_generation(~indicate=false), current, ancestors)
     };
   };
   */

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
    | Atom(s, _) => text(m, s.string)
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
// let measured_fold = (~annot: (measurement, ann_cat, 'acc) => 'acc, ~origin=0) =>
//   measured_fold'(~annot=(k, m, ann, l) => annot(m, ann, k(l)), ~origin);
// let find_space =
//     (~origin=0, n: Path.caret_step, l: t): (Color.t, measurement) => {
//   l
//   |> measured_fold'(
//        ~origin,
//        ~text=(_, _) => [],
//        ~cat=_ => List.concat,
//        ~annot=
//          (_k, measurement, _annot, l) =>
//            switch (l) {
//            | Atom(_, Space(m, color)) when m == n => [(color, measurement)]
//            | _ => []
//            },
//      )
//   |> ListUtil.hd_opt
//   |> OptUtil.get_or_raise(Invalid_argument("Layout.find_space"));
// };
// let find_range =
//     (~origin=0, (l, r): (Path.caret_step, Path.caret_step), layout: t) => {
//   let (color_l, l) = find_space(~origin, l, layout);
//   let (color_r, r) = find_space(~origin, r, layout);
//   let measurement = {origin: l.origin, length: r.origin - l.origin};
//   ((color_l, color_r), measurement);
// };
// let find_piece = (~origin=0, _step: Path.piece_step, l: t) =>
//   l
//   |> measured_fold'(
//        ~origin,
//        ~text=(_, _) => [],
//        ~cat=_ => List.concat,
//        ~annot=
//          (_k, _measurement, annot, _l) =>
//            switch (annot) {
//            //| Piece({step: s, color, shape}) when s == step => [
//            //    (measurement, color, shape, l),
//            //  ]
//            | _ => []
//            },
//      )
//   |> ListUtil.hd_opt
//   |> OptUtil.get_or_raise(Invalid_argument("Layout.find_piece"));
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
      (_k, _m, annot, _l) =>
        switch (annot) {
        //| Child({step: _, sort: (s_out, s_in)}) =>
        //  s_out == s_in
        //    ? ([{origin, length}], []) : ([], [{origin, length}])
        | _ => ([], [])
        },
  ) /* }*/;
// //let piece_holes =
// //  measured_fold(
// //    ~text=(_, _) => [],
// //    ~cat=_ => List.concat,
// //    ~annot=
// //      ({origin, _}, annot, holes) =>
// //        switch (annot) {
// //        | EmptyHole(sort, tip) => [(origin, sort, tip), ...holes]
// //        | _ => holes
// //        },
// //  );
// //let mk_Paren = (sort, body) =>
// //  cat([paren_l, open_child(sort, ChildStep.paren_body, body), paren_r]);
// //let mk_Ap = arg => cat([ap_l, open_child(Exp, ChildStep.ap_arg, arg), ap_r]);
// //let mk_Lam = (p, body) =>
// //  cat([
// //    lam_lam,
// //    closed_child((Exp, Pat), ChildStep.lam_pat, p),
// //    lam_open,
// //    open_child(Exp, ChildStep.lam_body, body),
// //    lam_close,
// //  ]);
// //let mk_Let = (p, def) => {
// //  cat([
// //    delim("let"),
// //    closed_child((Exp, Pat), ChildStep.let_pat, p),
// //    delim("="),
// //    open_child(Exp, ChildStep.let_def, def),
// //    delim("in"),
// //  ]);
// //};
// //let mk_Cond = then_ =>
// //  cat([
// //    delim("?"),
// //    open_child(Exp, ChildStep.cond_then, then_),
// //    delim(":"),
// //  ]);
// //let mk_Fact = () => Text("!");
// //let mk_Plus = () => Text("+");
// //let mk_Minus = () => Text("-");
// //let mk_Times = () => Text("*");
// //let mk_Div = () => Text("/");
// //let mk_Prod = () => Text(",");
// //let mk_OpHole = empty_hole;
// //let mk_BinHole = empty_hole;
// // let piece_shape = (piece: Piece.t) => {
// //   let (lshape, _) = Piece.tip(Left, piece);
// //   let (rshape, _) = Piece.tip(Right, piece);
// //   let ltails = Piece.tails(Left, piece);
// //   let rtails = Piece.tails(Right, piece);
// //   ((lshape, ltails), (rshape, rtails));
// // };
// // let rec mk_tiles = (~offset=0, ~rail_color as _=?, ts) =>
// //   List.mapi(
// //     (i, tile) => {
// //       let l_tile = mk_tile(tile);
// //       annot(
// //         Piece({
// //           step: offset + i,
// //           color: Color.of_sort(Tile.sort(tile)),
// //           shape: piece_shape(Tile(tile)),
// //         }),
// //         l_tile,
// //       );
// //     },
// //     ts,
// //   )
// // and mk_tile = t =>
// //   t
// //   |> Tile.get(
// //        fun
// //        | Tile_pat.OpHole => mk_OpHole(Pat, Convex)
// //        | Var(x) => mk_text(x)
// //        | Paren(body) =>
// //          // TODO undo unnecessary rewrapping
// //          mk_Paren(Pat, pad_spaces(Pat, mk_tiles(Tiles.of_pat(body))))
// //        | BinHole => mk_BinHole(Pat, Concave)
// //        | Prod => mk_Prod(),
// //        fun
// //        | Tile_exp.OpHole => mk_OpHole(Exp, Convex)
// //        | Num(n) => mk_text(string_of_int(n))
// //        | Var(x) => mk_text(x)
// //        | Paren(body) =>
// //          mk_Paren(Exp, pad_spaces(Exp, mk_tiles(Tiles.of_exp(body))))
// //        | Ap(arg) => mk_Ap(pad_spaces(Exp, mk_tiles(Tiles.of_exp(arg))))
// //        | Lam(p, body) =>
// //          mk_Lam(
// //            pad_spaces(Pat, mk_tiles(Tiles.of_pat(p))),
// //            pad_spaces(Exp, mk_tiles(Tiles.of_exp(body))),
// //          )
// //        | Let(p, def) =>
// //          mk_Let(
// //            pad_spaces(Pat, mk_tiles(Tiles.of_pat(p))),
// //            pad_spaces(Exp, mk_tiles(Tiles.of_exp(def))),
// //          )
// //        | Fact => mk_Fact()
// //        | BinHole => mk_BinHole(Exp, Concave)
// //        | Plus => mk_Plus()
// //        | Minus => mk_Minus()
// //        | Times => mk_Times()
// //        | Div => mk_Div()
// //        | Prod => mk_Prod()
// //        | Cond(then_) =>
// //          mk_Cond(pad_spaces(Exp, mk_tiles(Tiles.of_exp(then_)))),
// //      );
// // let mk_token =
// //   Shard.get(
// //     fun
// //     | Shard_pat.Paren_l => paren_l
// //     | Paren_r => paren_r,
// //     fun
// //     | Shard_exp.Paren_l => paren_l
// //     | Paren_r => paren_r
// //     | Ap_l => ap_l
// //     | Ap_r => ap_r
// //     | Lam_lam => lam_lam
// //     | Lam_open => lam_open
// //     | Lam_lam_open(p) =>
// //       cats([
// //         lam_lam,
// //         closed_child(
// //           (Exp, Pat),
// //           ChildStep.lam_pat,
// //           pad_spaces(Pat, mk_tiles(Tiles.of_pat(p))),
// //         ),
// //         lam_open,
// //       ])
// //     | Lam_close => lam_close
// //     | Let_let => delim("let")
// //     | Let_eq => delim("=")
// //     | Let_let_eq(p) =>
// //       cats([
// //         delim("let"),
// //         closed_child(
// //           (Exp, Pat),
// //           ChildStep.let_pat,
// //           pad_spaces(Pat, mk_tiles(Tiles.of_pat(p))),
// //         ),
// //         delim("="),
// //       ])
// //     | Let_in => delim("in")
// //     | Cond_que => delim("?")
// //     | Cond_col => delim(":"),
// //   );
// // let mk_piece = (step, piece: Piece.t) => {
// //   let l = Piece.get(mk_token, mk_tile, piece);
// //   let shape = piece_shape(piece);
// //   let color = Color.of_sort(Piece.sort(piece));
// //   annot(Piece({step, shape, color}), l);
// // };
// // let mk_selection = (~offset=0, ~frame_color: Color.t, selection) =>
// //   switch (Selection.tip_sorts(selection)) {
// //   | None => space(offset, frame_color)
// //   | Some((sort_l, _)) =>
// //     selection
// //     |> List.mapi((i, piece) =>
// //          [
// //            mk_piece(offset + i, piece),
// //            space(
// //              offset + i + 1,
// //              Color.of_sort(snd(Piece.tip(Right, piece))),
// //            ),
// //          ]
// //        )
// //     |> List.flatten
// //     |> List.cons(space(offset, Color.of_sort(sort_l)))
// //     |> cats
// //   };
// // let mk_relem = (~step, ~frame_color, relem: Restructuring.frame_elem) =>
// //   switch (relem) {
// //   | Tile(tile) => mk_piece(step, Tile(tile))
// //   | Selection(selection) =>
// //     mk_selection(~offset=step, ~frame_color, selection)
// //   };
// // let rec mk_frame = (subject: t, frame: Frame.t): t => {
// //   let mk_tiles_pat = (~offset=0, ts) =>
// //     mk_tiles(~offset, List.map(Tile.pat, ts));
// //   let mk_tiles_exp = (~offset=0, ts) =>
// //     mk_tiles(~offset, List.map(Tile.exp, ts));
// //   let mk_frame_pat =
// //       ((tile, shape), ((prefix, suffix), frame): Frame_pat.s) => {
// //     let step = List.length(prefix);
// //     let ls_prefix = mk_tiles_pat(List.rev(prefix));
// //     let ls_suffix = mk_tiles_pat(~offset=step + 1, suffix);
// //     let piece_ann = Piece({step, shape, color: Pat});
// //     mk_frame(
// //       pad_spaces(Pat, ls_prefix @ [annot(piece_ann, tile), ...ls_suffix]),
// //       Pat(frame),
// //     );
// //   };
// //   let mk_frame_exp =
// //       ((tile, shape), ((prefix, suffix), frame): Frame_exp.s) => {
// //     let step = List.length(prefix);
// //     let ls_prefix = mk_tiles_exp(List.rev(prefix));
// //     let ls_suffix = mk_tiles_exp(~offset=step + 1, suffix);
// //     let piece_ann = Piece({step, shape, color: Exp});
// //     mk_frame(
// //       pad_spaces(Exp, ls_prefix @ [annot(piece_ann, tile), ...ls_suffix]),
// //       Exp(frame),
// //     );
// //   };
// //   let shape_op = Nib.((Convex, 0), (Convex, 0));
// //   let shape_pre = Nib.((Convex, 0), (Concave, 0));
// //   let shape_post = Nib.((Concave, 0), (Convex, 0));
// //   let shape_bin = Nib.((Concave, 0), (Concave, 0));
// //   switch (frame) {
// //   | Pat(Paren_body(frame_s)) =>
// //     let tile = mk_Paren(Pat, subject);
// //     mk_frame_pat((tile, shape_op), frame_s);
// //   | Pat(Lam_pat(body, frame_s)) =>
// //     let tile = mk_Lam(subject, pad_spaces(Exp, mk_tiles_exp(body)));
// //     mk_frame_exp((tile, shape_op), frame_s);
// //   | Pat(Let_pat(def, frame_s)) =>
// //     let tile = mk_Let(subject, pad_spaces(Exp, mk_tiles_exp(def)));
// //     mk_frame_exp((tile, shape_pre), frame_s);
// //   | Exp(Paren_body(frame_s)) =>
// //     let tile = mk_Paren(Exp, subject);
// //     mk_frame_exp((tile, shape_op), frame_s);
// //   | Exp(Ap_arg(frame_s)) =>
// //     let tile = mk_Ap(subject);
// //     mk_frame_exp((tile, shape_post), frame_s);
// //   | Exp(Lam_body(p, frame_s)) =>
// //     let tile = mk_Lam(pad_spaces(Pat, mk_tiles_pat(p)), subject);
// //     mk_frame_exp((tile, shape_op), frame_s);
// //   | Exp(Let_def(p, frame_s)) =>
// //     let tile = mk_Let(pad_spaces(Pat, mk_tiles_pat(p)), subject);
// //     mk_frame_exp((tile, shape_pre), frame_s);
// //   | Exp(Cond_then(frame_s)) =>
// //     let tile = mk_Cond(subject);
// //     mk_frame_exp((tile, shape_bin), frame_s);
// //   | Exp(Root) => subject
// //   };
// // };
// // let mk_pointing = (sframe: Selection.frame, frame: Frame.t) => {
// //   let color = Color.of_sort(Frame.sort(frame));
// //   let subject =
// //     ListFrame.to_list(sframe)
// //     |> List.mapi((i, piece) => {mk_piece(i, piece)})
// //     |> pad_spaces(color);
// //   mk_frame(subject, frame);
// // };
// // let mk_selecting =
// //     (selection: Selection.t, sframe: Selection.frame, frame: Frame.t) => {
// //   let subject =
// //     ListFrame.to_list(~subject=selection, sframe)
// //     |> List.mapi((i, piece) =>
// //          [
// //            mk_piece(i, piece),
// //            space(i + 1, Color.of_sort(snd(Piece.tip(Right, piece)))),
// //          ]
// //        )
// //     |> List.flatten
// //     |> List.cons(space(0, Color.of_sort(Frame.sort(frame))))
// //     |> cats;
// //   mk_frame(subject, frame);
// // };
// // let mk_restructuring =
// //     (
// //       _backpack: Restructuring.Backpack.t,
// //       rframe: Restructuring.frame,
// //       frame: Frame.t,
// //     ) => {
// //   let frame_sort = Frame.sort(frame);
// //   let subject =
// //     Restructuring.get_sframe(rframe)
// //     |> ListFrame.to_list
// //     |> List.mapi((i, piece) =>
// //          [
// //            mk_piece(i, piece),
// //            space(i + 1, Color.of_sort(snd(Piece.tip(Right, piece)))),
// //          ]
// //        )
// //     |> List.flatten
// //     |> List.cons(space(0, Color.of_sort(frame_sort)))
// //     |> cats;
// //   mk_frame(subject, frame);
// // };
// // let mk_zipper =
// //   Memo.memoize((zipper: Zipper.t) =>
// //     switch (zipper) {
// //     | (Pointing(sframe), frame) => mk_pointing(sframe, frame)
// //     | (Selecting(_, selection, sframe), frame) =>
// //       mk_selecting(selection, sframe, frame)
// //     | (Restructuring((backpack, rframe)), frame) =>
// //       mk_restructuring(backpack, rframe, frame)
// //     }
// //   );
// // let mk_subject = ((down, up): Subject.t) => {
// //   ListFrame.to_list(~subject=selection, sframe)
// //   |> List.mapi((i, piece) =>
// //        [
// //          mk_piece(i, piece),
// //          space(i + 1, Color.of_sort(snd(Piece.tip(Right, piece)))),
// //        ]
// //      )
// //   |> List.flatten
// //   |> List.cons(space(0, Color.of_sort(Frame.sort(frame))))
// //   |> cats;
