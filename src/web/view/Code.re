open Virtual_dom.Vdom;
open Node;
open Tylr_core;
open Stds;

let view_text = (c: Cell.t) =>
  Layout.mk_cell(c)
  |> LCell.flatten
  |> Text.view_block
  |> Lists.single
  |> Node.span(~attrs=[Attr.class_("code-text")]);

let rec carets = (~font, c: Cell.t) => {
  switch (c.marks.cursor) {
  | None => []
  | Some(Point({hand, path})) =>
    let tree = Layout.mk_cell(c);
    let (state, _) = Layout.state_of_path(~tree, path);
    let z = Option.get(Zipper.unzip(c));
    [Dec.Caret.(mk(~font, Profile.mk(~loc=state.loc, hand, z.ctx)))];
  | Some(Select(sel)) =>
    let (l, r) = Path.Selection.carets(sel);
    let (l, r) = Cell.(put_cursor(Point(l), c), put_cursor(Point(r), c));
    carets(~font, l) @ carets(~font, r);
  };
};

let cursor = (~font, z: Zipper.t) => {
  let c = Zipper.zip(~save_cursor=true, z);
  let lc = Layout.mk_cell(c);

  let (_, ind_ctx) = Zipper.zip_indicated(z);
  let ind_cur =
    Option.get(c.marks.cursor)
    |> Cursor.map(
         Caret.map(Fun.const(Zipper.path_of_ctx(ind_ctx))),
         Fun.id,
       );
  let ind_lz = Layout.unzip(ind_cur, lc);
  let state = Layout.state_of_ctx(ind_lz.ctx);

  switch (ind_lz.cur) {
  | Point(ind_lc) =>
    switch (ind_lc.meld) {
    | None => []
    | Some(lm) =>
      Dec.Meld.Profile.mk(~whole=lc, ~state, lm) |> Dec.Meld.mk(~font)
    }
  | Select(_) => []
  };
};

// let cursor = (~font, z: Zipper.t) =>
//   switch (z.cur) {
//   | Select(_) => []
//   | Point(_) =>
//     let tree = Layout.mk_cell(Zipper.zip(~save_cursor=true, z));
//     let (cell, ctx) = Zipper.zip_indicated(z);
//     switch (Cell.get(cell)) {
//     | None => []
//     | Some(m) =>
//       let path = Zipper.path_of_ctx(ctx);
//       m |> Dec.Meld.Profile.mk(~tree, ~path) |> Dec.Meld.mk(~font);
//     };
//   };

let view = (~font: Model.Font.t, ~zipper: Zipper.t): Node.t => {
  // print_endline("--- Code.view ---");
  // print_endline("z = " ++ Zipper.show(zipper));
  let c = Zipper.zip(~save_cursor=true, zipper);
  // print_endline("c = " ++ Cell.show(c));
  // let t = Layout.mk_cell(c);
  // print_endline("t = " ++ LCell.show(t));
  // let b = LCell.flatten(t);
  // print_endline("b = " ++ Block.show(b));
  div(
    ~attrs=[Attr.class_("code"), Attr.id("under-the-rail")],
    [view_text(c), ...cursor(~font, zipper)] @ carets(~font, c),
  );
};
