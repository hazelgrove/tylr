open Virtual_dom.Vdom;
open Node;
open Core;
open Util;

let span_c = cls => span([Attr.class_(cls)]);

module Text = {
  let rec of_segment = (seg: Segment.t): list(Node.t) =>
    seg
    |> List.map(
         fun
         | Piece.Whitespace(w) => [
             Node.text(w.content == " " ? Unicode.nbsp : w.content),
           ]
         | Grout(_) => [Node.text(Unicode.nbsp)]
         | Tile(t) => of_tile(t),
       )
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
  let text_view = Text.of_segment(content);
  div([Attr.classes(["code-text", "backpack-selection"])], text_view);
};

let selection_length = (sel: Selection.t): int => {
  let seg = sel.content;
  let (len, _) = Measured.of_segment(seg);
  len;
  // switch (ListUtil.hd_opt(seg), ListUtil.last_opt(seg)) {
  // | (None, _)
  // | (_, None) => 0
  // | (Some(first), Some(last)) =>
  //   let first = Measured.find_p(first, map);
  //   let last = Measured.find_p(last, map);
  //   last.origin + last.length - first.origin;
  // };
};

let genie_profile =
    (backpack: Backpack.t, origin: int): RestructuringGenieDec.Profile.t => {
  length: backpack |> List.map(selection_length) |> List.fold_left(max, 2),
  origin,
};

let backpack_view =
    (~font_metrics: FontMetrics.t, ~origin, backpack: Backpack.t): Node.t => {
  let style =
    Printf.sprintf(
      "position: absolute; left: %fpx; bottom: %fpx;",
      (Float.of_int(origin) -. 0.0) *. font_metrics.col_width,
      2. *. font_metrics.row_height,
    );
  let selections_view =
    div(
      [Attr.create("style", style), Attr.classes(["backpack"])],
      List.map(backpack_sel_view, List.rev(backpack)),
    );
  let genie_profile = genie_profile(backpack, origin);
  let genie_view = RestructuringGenieDec.view(~font_metrics, genie_profile);
  div([Attr.classes(["backpack"])], [selections_view, genie_view]);
};

let segment_shape = (d: Direction.t, segment: Segment.t): Nib.Shape.t => {
  let (_, shape, _) = Segment.shape_affix(d, segment, Nib.Shape.concave());
  d == Right ? Nib.Shape.flip(shape) : shape;
};

let indicated_piece =
    (
      {
        relatives: {siblings: (l_sibs, r_sibs), ancestors},
        selection: {content, _},
        _,
      } as _z: Zipper.t,
    )
    : option((Piece.t, Nib.Shape.t)) =>
  switch (content, r_sibs, ancestors, l_sibs) {
  | ([_, ..._], _, _, _) => None
  | ([], [Whitespace(_), ..._], _, [_, ..._])
      when !Piece.is_whitespace(ListUtil.last(l_sibs)) =>
    // skip whitespace
    Some((ListUtil.last(l_sibs), segment_shape(Left, l_sibs)))
  | ([], [Whitespace(_), ..._], [(parent, _), ..._], []) =>
    // skip whitespace
    Some((
      Base.Tile(Ancestor.zip(r_sibs, parent)),
      segment_shape(Right, r_sibs),
    ))
  | ([], [r_nhbr, ..._], _, _) =>
    Some((r_nhbr, segment_shape(Right, r_sibs)))
  | ([], [], [(parent, _), ..._], _) =>
    //TODO(andrew): does Convex here make sense?
    Some((Base.Tile(Ancestor.zip(l_sibs, parent)), Convex))
  | ([], [], [], [_, ..._]) =>
    Some((ListUtil.last(l_sibs), segment_shape(Left, l_sibs)))
  | ([], [], [], []) => None
  };

let sort_n_nibs: (Nib.Shape.t, Piece.t) => (Sort.t, Nibs.t) =
  // TODO(d) fix sorts
  (l, p) =>
    switch (p) {
    | Whitespace(_) => (Exp, Mold.of_whitespace({sort: Exp, shape: l}).nibs)
    | Grout(g) => (Exp, Mold.of_grout(g, Exp).nibs)
    | Tile(t) => (t.mold.out, Tile.nibs(t))
    };

let caret_shape =
    (
      {
        caret,
        selection: {content, focus},
        relatives: {siblings: (l_sibs, r_sibs), _},
        _,
      } as z: Zipper.t,
    )
    : CaretDec.caret_shape =>
  switch (indicated_piece(z)) {
  | _ when caret != Outer => Straight
  | _ when content != [] =>
    let shape =
      switch (focus) {
      | Left => segment_shape(Right, content @ r_sibs)
      | Right => segment_shape(Left, l_sibs @ content)
      };
    switch (shape) {
    | Convex => Right
    | Concave(_) => Left
    };
  | None => Straight
  | Some((p, shape)) =>
    let (_, (_shape_l, _shape_r)) = sort_n_nibs(shape, p);
    //TODO(andrew): bug... grout caret shape is wrong
    switch (shape) {
    | Convex => Right
    | Concave(_) => Left
    };
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
         | Grout(g) => {
             // TODO(d) fix sort
             let mold = Mold.of_grout(g, Exp);
             let measurement = Measured.find_g(g, M.map);
             [EmptyHoleDec.view(~font_metrics, {measurement, mold})];
           },
       )
    |> List.concat;

  let selection_profile = (z: Zipper.t): Layout.measurement => {
    let sel = z.selection.content;
    let length = Measured.length(sel, M.map);
    let origin =
      switch (Siblings.neighbors(z.relatives.siblings)) {
      | (_, Some(p)) =>
        let m = Measured.find_p(p, M.map);
        m.origin - length;
      | (Some(p), _) =>
        let m = Measured.find_p(p, M.map);
        m.origin + m.length;
      | (None, None) =>
        let p =
          ListUtil.hd_opt(sel) |> OptUtil.get_or_raise(Segment.Empty_segment);
        Measured.find_p(p, M.map).origin;
      };
    {origin, length};
  };

  let caret = (z: Zipper.t): list(Node.t) => {
    let profile = selection_profile(z);
    let origin =
      switch (z.selection.focus) {
      | Left => profile.origin
      | Right => profile.origin + profile.length
      };
    let caret_shape = caret_shape(z);
    let caret_offset =
      switch (z.caret) {
      | Outer => 0
      | Inner(n) => n + 1
      };
    [
      //TODO(andrew): restore?
      //SelectedBoxDec.view(~font_metrics, profile),
      CaretDec.simple_view(
        ~font_metrics,
        origin + caret_offset,
        "#f008",
        caret_shape,
      ),
      backpack_view(~font_metrics, ~origin, z.backpack),
    ];
  };

  let children = (p: Piece.t): list(Layout.measurement) =>
    switch (p) {
    | Whitespace(_)
    | Grout(_) => []
    | Tile(t) =>
      let m = Measured.find_t(t, M.map);
      let token = List.nth(t.label);
      Aba.mk(t.shards, t.children)
      |> Aba.fold_left(
           shard => (m.origin + Unicode.length(token(shard)), []),
           ((origin, children: list(Layout.measurement)), child, shard) => {
             let length = Measured.length(child, M.map);
             (
               origin + length + Unicode.length(token(shard)),
               children @ [{origin, length}],
             );
           },
         )
      |> snd;
    };

  let selected_pieces = (z: Zipper.t): list(Node.t) => {
    // TODO mold/nibs/selemdec clean up pass
    let (l, _) = Siblings.shapes(z.relatives.siblings);
    z.selection.content
    |> ListUtil.fold_left_map(
         (l: Nib.Shape.t, p: Piece.t) => {
           let m = Measured.find_p(p, M.map);
           let (sort: Sort.t, nibs) = sort_n_nibs(l, p);
           let profile =
             SelemDec.Profile.{
               color: Color.of_sort(sort),
               shape: Layout.piece_shape_of_nibs(nibs),
               measurement: m,
               style: Selected,
               closed_children: [],
               open_children: children(p),
             };
           (snd(nibs).shape, SelemDec.view(~font_metrics, profile));
         },
         l,
       )
    |> snd;
  };

  let indicated_piece_deco = (z: Zipper.t): list(Node.t) => {
    switch (indicated_piece(z)) {
    | None => []
    | Some((p, nib_shape)) =>
      let m = Measured.find_p(p, M.map);
      let (sort, nibs) = sort_n_nibs(nib_shape, p);
      let profile =
        SelemDec.Profile.{
          color: Color.of_sort(sort),
          shape: Layout.piece_shape_of_nibs(nibs),
          measurement: m,
          style: Root,
          closed_children: [],
          open_children:
            p |> children |> Layout.relativize_measurements(m.origin),
        };
      [SelemDec.view(~font_metrics, profile)];
    };
  };

  let rec targets = (bp: Backpack.t, seg: Segment.t) => {
    let root_targets =
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
               | (_, Some(p)) =>
                 let m = Measured.find_p(p, M.map);
                 Layout.{origin: m.origin, length: 1};
               | (Some(p), _) =>
                 let m = Measured.find_p(p, M.map);
                 Layout.{origin: m.origin + m.length, length: 1};
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
  module Deco =
    Deco({
      let font_metrics = font_metrics;
      let map = snd(Measured.of_segment(seg));
    });
  div(
    [Attr.class_("code"), Attr.id("under-the-rail")],
    [span_c("code-text", Text.of_segment(seg))] @ Deco.all(zipper),
  );
};
