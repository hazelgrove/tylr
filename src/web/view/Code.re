open Virtual_dom.Vdom;
open Node;
open Core;

let span_c = cls => span([Attr.class_(cls)]);

let code_text =
    (unsel_seg, sel_seg, map, ~settings: Model.settings): list(Node.t) => {
  module Text =
    CodeText.Text({
      let map = map;
      let settings = settings;
    });
  [
    span_c("code-text", Text.of_segment(unsel_seg)),
    span_c("code-text-shards", Text.of_segment(sel_seg)),
  ];
};

let decos =
    (sel_seg, map, ~font_metrics, ~show_backpack_targets, ~zipper: Zipper.t)
    : list(Node.t) => {
  module Deco =
    Deco.Deco({
      let font_metrics = font_metrics;
      let map = map;
      let show_backpack_targets = show_backpack_targets;
    });
  List.concat([
    Deco.holes(sel_seg),
    Deco.caret(zipper),
    Deco.indicated_piece_deco(zipper),
    Deco.selected_pieces(zipper),
    Deco.backback(zipper),
    Deco.targets'(zipper.backpack, sel_seg),
  ]);
};

let view =
    (
      ~font_metrics,
      ~show_backpack_targets,
      ~zipper: Zipper.t,
      ~settings: Model.settings,
    )
    : Node.t => {
  let sel_seg = Zipper.zip(zipper);
  let unsel_seg = Zipper.unselect_and_zip(zipper);
  let map = Measured.of_segment(unsel_seg);
  div(
    [Attr.class_("code"), Attr.id("under-the-rail")],
    code_text(unsel_seg, sel_seg, map, ~settings)
    @ decos(sel_seg, map, ~font_metrics, ~show_backpack_targets, ~zipper),
  );
};
