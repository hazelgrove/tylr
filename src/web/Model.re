open Core;

type t = {
  zipper: Zipper.t,
  history: ActionHistory.t,
  font_metrics: FontMetrics.t,
  logo_font_metrics: FontMetrics.t,
  show_neighbor_tiles: bool,
};

let cutoff = (===);

let mk_atom = (sort, name): Tile.t => {
  label: [name],
  mold: Mold.(mk_op(Sorts.mk(sort))),
  children: [],
};

let mk_exp_atom = mk_atom(Exp);
let mk_pat_atom = mk_atom(Pat);
let one = mk_exp_atom("1");
let two = mk_exp_atom("2");
let pat_foo = mk_pat_atom("foo");
let exp_foo = mk_exp_atom("foo");
let pat_bar = mk_pat_atom("bar");
let pat_taz = mk_pat_atom("taz");

let plus_12: Tile.t = {
  label: ["+"],
  mold: Mold.(mk_bin(Precedence.plus, Sorts.mk(Exp))),
  children: [],
};

let segment_of_tiles: list(Tile.t) => Segment.t =
  List.map(t => Piece.Tile(t));

let paren_plus12: Tile.t = {
  label: ["(", ")"],
  mold: Mold.(mk_op(Sorts.mk(Exp))),
  children: [segment_of_tiles([one, plus_12, two])],
};

let mk_lambda_ancestor:
  (list(list(Piece.t)), list(list(Piece.t))) => Ancestor.t =
  (left, right) => {
    label: ["λ", "{", "}"],
    mold: Mold.(mk_op(Sorts.mk(~in_=[Pat, Exp], Exp))),
    children: (left, right),
  };

let mk_empty_sibs: Ancestor.t => (Ancestor.t, Siblings.t) =
  ancestor => (ancestor, ([], []));

let inner_stx: Segment.t = [Tile(paren_plus12)];

let ancestors: Ancestors.t = [
  mk_empty_sibs(mk_lambda_ancestor([[Tile(pat_bar)]], [])),
  mk_empty_sibs(mk_lambda_ancestor([[Tile(pat_taz)]], [])),
];

let init = () => {
  zipper:
    Zipper.{
      selection: {
        focus: Left,
        content: Segment.empty,
      },
      backpack: Backpack.empty,
      relatives: {
        siblings: (Siblings.Prefix.empty, inner_stx),
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
