open Virtual_dom.Vdom;
open Node;
open Tylr_core;
open Stds;

let view_text = (c: Cell.t) =>
  Layout.Tree.of_cell(c)
  |> Layout.Tree.flatten
  |> Text.view_block
  |> Lists.single
  |> Node.span(~attrs=[Attr.class_("code-text")]);

let rec carets = (~font, c: Cell.t) => {
  switch (c.marks.cursor) {
  | None => []
  | Some(Point({hand, path})) =>
    let tree = Layout.Tree.of_cell(c);
    let (state, _) = Layout.state_of_path(~tree, path);
    let z = Option.get(Zipper.unzip(c));
    [Dec.Caret.(mk(~font, Profile.mk(~loc=state.loc, hand, z.ctx)))];
  | Some(Select(sel)) =>
    let (l, r) = Path.Selection.carets(sel);
    let (l, r) = Cell.(put_cursor(Point(l), c), put_cursor(Point(r), c));
    carets(~font, l) @ carets(~font, r);
  };
};

let cursor = (~font, z: Zipper.t) =>
  switch (z.cur) {
  | Select(_) => []
  | Point(_) =>
    let tree = Layout.Tree.of_cell(Zipper.zip(~save_cursor=true, z));
    let (cell, ctx) = Zipper.zip_indicated(z);
    switch (Cell.get(cell)) {
    | None => []
    | Some(m) =>
      let path = Zipper.path_of_ctx(ctx);
      m |> Dec.Meld.Profile.mk(~tree, ~path) |> Dec.Meld.mk(~font);
    };
  };

let view = (~font: Model.Font.t, ~zipper: Zipper.t): Node.t => {
  // print_endline("--- Code.view ---");
  // print_endline("z = " ++ Zipper.show(zipper));
  let c = Zipper.zip(~save_cursor=true, zipper);
  // print_endline("c = " ++ Cell.show(c));
  // let t = Layout.Tree.of_cell(c);
  // print_endline("t = " ++ Layout.Tree.show(t));
  // let b = Layout.Tree.flatten(t);
  // print_endline("b = " ++ Layout.Block.show(b));
  div(
    ~attrs=[Attr.class_("code"), Attr.id("under-the-rail")],
    [view_text(c), ...cursor(~font, zipper)] @ carets(~font, c),
  );
};
