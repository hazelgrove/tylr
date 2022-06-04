open Core;

type t = {
  zipper: Zipper.t,
  id_gen: IdGen.state,
  history: ActionHistory.t,
  font_metrics: FontMetrics.t,
  logo_font_metrics: FontMetrics.t,
  show_neighbor_tiles: bool,
};

let cutoff = (===);

let blank = {
  {
    id_gen: 1,
    zipper: {
      selection: {
        focus: Left,
        content: [],
      },
      backpack: [],
      relatives: {
        siblings: ([Grout({id: 0, shape: Convex})], []),
        ancestors: [],
      },
      caret: Outer,
      caret_col_target: 0,
    },
    history: ActionHistory.empty,
    font_metrics: FontMetrics.init,
    logo_font_metrics: FontMetrics.init,
    show_neighbor_tiles: false,
  };
};
