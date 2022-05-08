open Core;

type t = {
  zipper: Zipper.t,
  history: ActionHistory.t,
  font_metrics: FontMetrics.t,
  logo_font_metrics: FontMetrics.t,
  show_neighbor_tiles: bool,
};

let cutoff = (===);

let mk_atom = (id, sort, name): Tile.t => {
  id,
  label: [name],
  mold: Mold.(mk_op(Sorts.mk(sort))),
  children: [],
};

let mk_exp_atom = id => mk_atom(id, Exp);
let mk_pat_atom = id => mk_atom(id, Pat);

let mk_infix_op = (id, s: string, p: Precedence.t): Tile.t => {
  id,
  label: [s],
  mold: Mold.(mk_bin(p, Sorts.mk(Exp))),
  children: [],
};

let mk_parens_exp = (id, children): Tile.t => {
  id,
  label: ["(", ")"],
  mold: Mold.(mk_op(Sorts.mk(~in_=[Exp], Exp))),
  children,
};

let mk_lambda_ancestor:
  (Id.t, list(list(Piece.t)), list(list(Piece.t))) => Ancestor.t =
  (id, left, right) => {
    id,
    label: ["λ", "{", "}"],
    mold: Mold.(mk_op(Sorts.mk(~in_=[Pat, Exp], Exp))),
    children: (left, right),
  };

let mk_singleton_generation: Ancestor.t => Ancestors.generation =
  ancestor => (ancestor, ([], []));

let one = Piece.Tile(mk_exp_atom(0, "1"));
let two = Piece.Tile(mk_exp_atom(1, "2"));
let exp_foo = Piece.Tile(mk_exp_atom(2, "foo"));
let pat_foo = Piece.Tile(mk_pat_atom(3, "foo"));
let pat_bar = Piece.Tile(mk_pat_atom(4, "bar"));
let pat_taz = Piece.Tile(mk_pat_atom(5, "taz"));
let plus = Piece.Tile(mk_infix_op(6, "+", Precedence.plus));
let paren_one_plus_two = Piece.Tile(mk_parens_exp(7, [[one, plus, two]]));

let l_sibling: Segment.t = [plus, Grout(Convex)];
let r_sibling: Segment.t = [paren_one_plus_two];

let content: Segment.t = [
  exp_foo,
  Grout(Concave),
  paren_one_plus_two,
  Grout(Concave),
];

let ancestors: Ancestors.t = [
  mk_singleton_generation(mk_lambda_ancestor(8, [[pat_bar]], [])),
  mk_singleton_generation(mk_lambda_ancestor(9, [[pat_taz]], [])),
];

let backpack: list(Selection.t) = [{focus: Left, content: [exp_foo]}];

let init = () => {
  zipper: {
    id_gen: 10,
    selection: {
      focus: Left,
      content,
    },
    backpack,
    relatives: {
      siblings: (l_sibling, r_sibling),
      ancestors,
    },
  },
  history: ActionHistory.empty,
  font_metrics: FontMetrics.init,
  logo_font_metrics: FontMetrics.init,
  show_neighbor_tiles: false,
};

// let filler = (model: t) => {
//   switch (model.zipper) {
//   | (Pointing(_) | Selecting(_), _) => 0
//   | (Restructuring(_), _) =>
//     switch (ActionHistory.zipper_before_restructuring(model.history)) {
//     | None => 0
//     | Some(zipper) =>
//       let len_before = Layout.length(Layout.mk_zipper(zipper));
//       let len_now = Layout.length(Layout.mk_zipper(model.zipper));
//       len_now < len_before ? len_before - len_now : 0;
//     }
//   };
// };
let filler = _ => 0;
