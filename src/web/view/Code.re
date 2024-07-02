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

let cursor = (~font, z: Zipper.t) =>
  switch (z.cur) {
  | Select(_) =>
    print_endline("0");
    [];
  | Point(_) =>
    let tree = Layout.Tree.of_cell(Zipper.zip(~save_cursor=true, z));
    let (cell, ctx) = Zipper.zip_indicated(z);
    switch (Layout.state_of_path(~tree, Zipper.path_of_ctx(ctx))) {
    | (state, Some(t)) =>
      switch (Cell.get(cell), t) {
      | (Some(m), Some(lyt)) when !Cell.Space.is_space(cell) =>
        Dec.Meld.(mk(~font, Profile.mk(~state, lyt, m)))
      | _ => []
      }
    | _ => []
    };
  };

let view = (~font: Model.Font.t, ~zipper: Zipper.t): Node.t => {
  let c = Zipper.zip(~save_cursor=true, zipper);
  div(
    ~attrs=[Attr.class_("code"), Attr.id("under-the-rail")],
    [view_text(c), ...cursor(~font, zipper)],
  );
};
