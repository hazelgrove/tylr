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
let int = n => mk_monotile(Form.int_exp(n));
let exp = v => mk_monotile(Form.var_exp(v));
let pat = v => mk_monotile(Form.var_pat(v));
let mk_parens_exp = mk_tile(Form.get("parens_exp"));
let mk_fun = mk_tile(Form.get("fun_"));
let mk_fun_ancestor = mk_ancestor(Form.get("fun_"));
let mk_parens_ancestor = mk_ancestor(Form.get("parens_exp"));
let mk_let_ancestor = mk_ancestor(Form.get("let_"));
let plus = mk_monotile(Form.get("plus"));

let l_sibling: Segment.t = [plus, Grout(Convex)];
let r_sibling: Segment.t = [mk_parens_exp([[int(1), plus, int(2)]])];

let content: Segment.t = [exp("foo"), Grout(Concave)];

let ancestors: Ancestors.t = [
  (mk_parens_ancestor(([], [])), ([mk_fun([[pat("bar")]])], [])),
  (mk_parens_ancestor(([], [])), ([mk_fun([[pat("taz")]])], [])),
  (mk_let_ancestor(([[pat("foo")]], [])), ([], [int(2)])),
];

let backpack: Backpack.t = [{focus: Left, content: [exp("foo")]}];

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
let blank = {
  zipper: {
    id_gen: id_gen^,
    selection: {
      focus: Left,
      content: [],
    },
    backpack: [],
    relatives: {
      siblings: ([Grout(Convex)], []),
      ancestors: [],
    },
    caret: Outer,
  },
  history: ActionHistory.empty,
  font_metrics: FontMetrics.init,
  logo_font_metrics: FontMetrics.init,
  show_neighbor_tiles: false,
};
