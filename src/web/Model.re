open Core;

type t = {
  zipper: Zipper.t,
  history: ActionHistory.t,
  font_metrics: FontMetrics.t,
  logo_font_metrics: FontMetrics.t,
  show_neighbor_tiles: bool,
};

let cutoff = (===);

let id_gen: ref(int) = ref(0);

let mk_id = (): int => {
  let uid = id_gen^;
  id_gen := id_gen^ + 1;
  uid;
};

let mk_tile: (Form.t, list(list(Piece.t))) => Piece.t =
  //TODO: asserts
  (form, children) =>
    Tile({id: mk_id(), label: form.label, mold: form.mold, children});

let mk_ancestor:
  (Form.t, (list(list(Piece.t)), list(list(Piece.t)))) => Ancestor.t =
  //TODO: asserts
  (form, children) => {
    id: mk_id(),
    label: form.label,
    mold: form.mold,
    children,
  };

let mk_monotile = form => mk_tile(form, []); //TODO: asserts

let mk_parens_exp = mk_tile(Form.parens_exp);
let mk_lambda_exp = mk_tile(Form.lambda);
let mk_lambda_ancestor = mk_ancestor(Form.lambda);
let mk_parens_ancestor = mk_ancestor(Form.parens_exp);
let mk_let_ancestor = mk_ancestor(Form.let_);
let plus = mk_monotile(Form.plus);
let one = mk_monotile(Form.int_exp(1));
let two = mk_monotile(Form.int_exp(2));
let exp_foo = mk_monotile(Form.var_exp("foo"));
let pat_foo = mk_monotile(Form.var_pat("foo"));
let pat_bar = mk_monotile(Form.var_pat("bar"));
let pat_taz = mk_monotile(Form.var_pat("taz"));

let l_sibling: Segment.t = [plus, Grout((Convex, Convex))];
let r_sibling: Segment.t = [mk_parens_exp([[one, plus, two]])];

let content: Segment.t = [
  exp_foo,
  Grout((Concave(Precedence.min), Concave(Precedence.min))),
];

let ancestors: Ancestors.t = [
  (mk_parens_ancestor(([], [])), ([mk_lambda_exp([[pat_bar]])], [])),
  (mk_parens_ancestor(([], [])), ([mk_lambda_exp([[pat_taz]])], [])),
  (mk_let_ancestor(([[pat_foo]], [])), ([], [two])),
];

let backpack: list(Selection.t) = [{focus: Left, content: [exp_foo]}];

let init = () => {
  zipper: {
    id_gen: id_gen^,
    selection: {
      focus: Left,
      content,
    },
    backpack,
    relatives: {
      siblings: (l_sibling, r_sibling),
      ancestors,
    },
    caret: Outer,
  },
  history: ActionHistory.empty,
  font_metrics: FontMetrics.init,
  logo_font_metrics: FontMetrics.init,
  show_neighbor_tiles: false,
};

let filler = _ => 0;
