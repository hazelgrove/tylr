open Virtual_dom.Vdom;
open Node;
open Tylr_core;
open Stds;

let view_text = (~font, c: Cell.t) =>
  Layout.mk_cell(c)
  |> LCell.flatten
  |> Text.view_block(~font)
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
      Dec.Meld.Profile.mk(~whole=lc, ~state, lm)
      |> snd
      |> Dec.Layers.view(~font)
    }
  | Select(ind_zigg) =>
    let sel = Option.get(Cursor.get_select(z.cur));
    let rolled =
      Zigg.roll_bounds(
        ~l=Ctx.nonspace_face(~side=L, z.ctx),
        sel.range,
        ~r=Ctx.nonspace_face(~side=R, z.ctx),
      );
    let null = {
      let (dn, up) = Ctx.hd(z.ctx);
      let l = Zigg.is_null(~side=L, ~slope=dn, sel.range);
      let r = Zigg.is_null(sel.range, ~slope=up, ~side=R);
      (l, r);
    };
    let eqs = (
      snd(List.split(fst(ind_lz.eqs))),
      fst(List.split(snd(ind_lz.eqs))),
    );
    ind_zigg
    |> Dec.Zigg.Profile.mk(~whole=lc, ~state, ~null, ~eqs, ~rolled)
    |> Dec.Layers.view(~font);
  };
};

let view = (~font: Model.Font.t, ~zipper: Zipper.t): Node.t => {
  // P.log("--- Code.view ---");
  // P.show("z", Zipper.show(zipper));
  let c = Zipper.zip(~save_cursor=true, zipper);
  // P.show("c", Cell.show(c));
  // let t = Layout.mk_cell(c);
  // print_endline("t = " ++ LCell.show(t));
  // let b = LCell.flatten(t);
  // print_endline("b = " ++ Block.show(b));
  div(
    ~attrs=[Attr.class_("code"), Attr.id("under-the-rail")],
    [view_text(~font, c), ...cursor(~font, zipper)] @ carets(~font, c),
  );
};
