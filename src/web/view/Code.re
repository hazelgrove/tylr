open Virtual_dom.Vdom;
open Node;
open Core;
open Util;

let span_c = cls => span([Attr.class_(cls)]);

module Text = (M: {let map: Measured.t;}) => {
  let rec of_segment = (seg: Segment.t): list(Node.t) =>
    seg
    |> List.map((p: Piece.t) => {
         let m = Measured.find_p(p, M.map);
         switch (p) {
         | Piece.Whitespace(w) when w.content == Whitespace.linebreak => [
             span_c("whitespace", [text(Whitespace.linebreak)]),
             Node.br([]),
             Node.text(
               String.concat("", List.init(m.last.col, _ => Unicode.nbsp)),
             ),
           ]
         | Whitespace({content: c, _}) when c == Whitespace.space => [
             span_c("whitespace", [text("·")]),
           ]
         | Whitespace(w) => [Node.text(w.content)]
         | Grout(_) => [Node.text(Unicode.nbsp)]
         | Tile(t) => of_tile(t)
         };
       })
    |> List.concat
  and of_tile = (t: Tile.t): list(Node.t) => {
    let span =
      List.length(t.label) == 1
        ? Node.span([])
        : span_c(Tile.is_complete(t) ? "delim" : "extra-bold-delim");
    Aba.mk(t.shards, t.children)
    |> Aba.join(
         i => [span([Node.text(List.nth(t.label, i))])],
         of_segment,
       )
    |> List.concat;
  };
};

let backpack_sel_view = ({focus: _, content}: Selection.t): t => {
  // TODO(andrew): Maybe use sort at caret instead of root
  let (_, map) = Measured.of_segment(content);
  module Text =
    Text({
      let map = map;
    });
  let text_view = Text.of_segment(content);
  div(
    [Attr.classes(["code-text", "backpack-selection"])],
    [text(Unicode.nbsp)] @ text_view @ [text(Unicode.nbsp)],
  );
};

let backpack_view =
    (
      ~font_metrics: FontMetrics.t,
      ~origin: Measured.point,
      backpack: Backpack.t,
    )
    : Node.t => {
  let length =
    switch (backpack) {
    | [] => 0
    | [hd, ..._] => Measured.segment_width(hd.content) + 2 //space-padding
    };
  let height =
    List.fold_left(
      (acc, sel: Selection.t) => acc + Measured.segment_height(sel.content),
      0,
      backpack,
    );
  //TODO(andrew): truncate backpack when height is too high?
  let style =
    Printf.sprintf(
      "position: absolute; left: %fpx; top: %fpx;",
      Float.of_int(origin.col) *. font_metrics.col_width,
      Float.of_int(/* origin.row */ - height - 1)
      *. font_metrics.row_height
      +. CaretDec.top_text_fudge,
    );
  let selections_view =
    div(
      [Attr.create("style", style), Attr.classes(["backpack"])],
      List.map(backpack_sel_view, List.rev(backpack)),
    );

  let genie_profile = RestructuringGenieDec.Profile.{length, height, origin};
  let genie_view = RestructuringGenieDec.view(~font_metrics, genie_profile);
  div([Attr.classes(["backpack"])], [selections_view, genie_view]);
};

module Deco = (M: {
                 let font_metrics: FontMetrics.t;
                 let map: Measured.t;
               }) => {
  let font_metrics = M.font_metrics;

  let rec holes = (seg: Segment.t): list(Node.t) =>
    seg
    |> List.map(
         fun
         | Piece.Whitespace(_) => []
         | Tile(t) => t.children |> List.map(holes) |> List.concat
         | Grout(g) => [
             EmptyHoleDec.view(
               ~font_metrics, // TODO(d) fix sort
               {
                 measurement: Measured.find_g(g, M.map),
                 mold: Mold.of_grout(g, Exp),
               },
             ),
           ],
       )
    |> List.concat;

  let caret = (z: Zipper.t): list(Node.t) => {
    let origin = Zipper.caret_point(M.map, z);
    let side =
      switch (Zipper.indicated_piece(z)) {
      | Some((_, side)) => side
      | _ => Right
      };
    let style =
      Printf.sprintf(
        "position: absolute; left: %fpx; top: %fpx; height: %fpx;",
        Float.of_int(origin.col) *. font_metrics.col_width,
        CaretDec.top_text_fudge -. 3.,
        Float.of_int(origin.row) *. font_metrics.row_height +. 3.,
      );
    let joiner =
      div(
        [Attr.create("style", style), Attr.classes(["backpack-joiner"])],
        [],
      );
    [
      CaretDec.simple_view(
        ~font_metrics,
        ~side,
        ~origin,
        ~shape=Zipper.caret_direction(z),
      ),
      backpack_view(~font_metrics, ~origin, z.backpack),
    ]
    @ (z.backpack != [] ? [joiner] : []);
  };

  let children = (p: Piece.t): list(Measured.measurement_lin) =>
    switch (p) {
    | Whitespace(_)
    | Grout(_) => []
    | Tile(t) =>
      let m = Measured.find_t(t, M.map);
      let token = List.nth(t.label);
      Aba.mk(t.shards, t.children)
      |> Aba.fold_left(
           shard => (m.origin.col + Unicode.length(token(shard)), []),
           (
             (origin, children: list(Measured.measurement_lin)),
             child,
             shard,
           ) => {
             let length = Measured.length(child, M.map);
             (
               origin + length + Unicode.length(token(shard)),
               children @ [{origin, length}],
             );
           },
         )
      |> snd;
    };

  let piece_profile =
      (p: Piece.t, nib_shape: Nib.Shape.t, style: PieceDec.Style.t)
      : PieceDec.Profile.t => {
    // TODO(d) fix sorts
    let mold =
      switch (p) {
      | Whitespace(_) => Mold.of_whitespace({sort: Exp, shape: nib_shape})
      | Grout(g) => Mold.of_grout(g, Exp)
      | Tile(t) => t.mold
      };
    // TODO(d) awkward
    let shards =
      switch (p) {
      | Whitespace(w) => [(0, Measured.find_w(w, M.map))]
      | Grout(g) => [(0, Measured.find_g(g, M.map))]
      | Tile(t) => Measured.find_shards(t, M.map)
      };
    PieceDec.Profile.{shards, mold, style};
  };

  let selected_pieces = (z: Zipper.t): list(Node.t) =>
    // TODO(d) mold/nibs/selemdec clean up pass
    z.selection.content
    |> List.filter(
         fun
         | Piece.Whitespace(w) when w.content == Whitespace.linebreak => false
         | _ => true,
       )
    |> ListUtil.fold_left_map(
         (l: Nib.Shape.t, p: Piece.t) => {
           let profile = piece_profile(p, l, Selected);
           (
             snd(Mold.nibs(profile.mold)).shape,
             PieceDec.view(~font_metrics, ~rows=M.map.rows, profile),
           );
         },
         fst(Siblings.shapes(z.relatives.siblings)),
       )
    |> snd;

  let indicated_piece_deco = (z: Zipper.t): list(Node.t) => {
    switch (Zipper.indicated_piece(z)) {
    | None => []
    | Some((Whitespace(w), _)) when w.content == Whitespace.linebreak => []
    | Some((p, side)) =>
      let nib_shape =
        switch (Zipper.caret_direction(z)) {
        | None => Nib.Shape.Convex
        | Some(nib) => Nib.Shape.relative(nib, side)
        };
      [
        PieceDec.view(
          ~font_metrics,
          ~rows=M.map.rows,
          piece_profile(p, nib_shape, Root(Measured.zero, Measured.zero)),
        ),
      ];
    };
  };

  let rec targets = (bp: Backpack.t, seg: Segment.t) => {
    let root_targets =
      // TODO(d): review correctness wrt splits reversing prefix
      ListUtil.splits(seg)
      |> List.map(((l, r)) => {
           let sibs = Segment.(incomplete_tiles(l), incomplete_tiles(r));
           switch (Backpack.pop(sibs, bp)) {
           | None
           | Some((true, _, _)) => []
           | Some(_) =>
             let measurement =
               switch (Siblings.neighbors((l, r))) {
               | (None, None) => failwith("impossible")
               | (_, Some(p))
               | (Some(p), _) => Measured.find_p(p, M.map)
               };
             let profile =
               CaretPosDec.Profile.{
                 style: `Sibling,
                 measurement,
                 color: Color.Exp,
                 just_failed: None,
               };
             [CaretPosDec.view(~font_metrics, profile)];
           };
         })
      |> List.concat;
    switch (root_targets) {
    | [_, ..._] => root_targets
    | [] =>
      seg
      |> List.filter_map(
           fun
           | Piece.Tile(t) => Some(t)
           | _ => None,
         )
      |> List.map((t: Tile.t) =>
           List.concat(List.map(targets(bp), t.children))
         )
      |> List.concat
    };
  };

  let all = (z: Zipper.t) => {
    let seg = Zipper.zip(z);
    List.concat([
      holes(seg),
      caret(z),
      targets(z.backpack, seg),
      selected_pieces(z),
      indicated_piece_deco(z),
    ]);
  };
};

let view =
    (
      ~font_metrics,
      ~just_failed as _: option(FailedInput.t)=None,
      ~show_neighbor_tiles as _: bool=false,
      ~zipper: Zipper.t,
    )
    : Node.t => {
  let seg = Zipper.zip(zipper);
  let unselected = Zipper.unselect_and_zip(zipper);
  let map = snd(Measured.of_segment(unselected));

  module Text =
    Text({
      let map = map;
    });
  module Deco =
    Deco({
      let font_metrics = font_metrics;
      let map = map;
    });
  div(
    [Attr.class_("code"), Attr.id("under-the-rail")],
    [span_c("code-text", Text.of_segment(seg))] @ Deco.all(zipper),
  );
};
