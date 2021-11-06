open Core;

type t = {
  zipper: Zipper.t,
  history: ActionHistory.t,
  font_metrics: FontMetrics.t,
  logo_font_metrics: FontMetrics.t,
  show_neighbor_tiles: bool,
};

let cutoff = (===);

let init = () => {
  zipper: (
    Subject.Pointing(([], Selem.[Tile(Exp(OpHole))])),
    Frame.Exp(Root),
  ),
  history: ActionHistory.empty,
  font_metrics: FontMetrics.init,
  logo_font_metrics: FontMetrics.init,
  show_neighbor_tiles: false,
};

let filler = (model: t) => {
  switch (model.zipper) {
  | (Pointing(_) | Selecting(_), _) => 0
  | (Restructuring(_), _) =>
    switch (ActionHistory.zipper_before_restructuring(model.history)) {
    | None => 0
    | Some(zipper) =>
      let len_before = Layout.length(Layout.mk_zipper(zipper));
      let len_now = Layout.length(Layout.mk_zipper(model.zipper));
      len_now < len_before ? len_before - len_now : 0;
    }
  };
};
