open Virtual_dom.Vdom;

let px = f => Printf.sprintf("%fpx", f);

let mk = (~font, ~loc: Tylr_core.Loc.t) => {
  let (row, col) = Model.Font.(row(font), col(font));
  Util.Nodes.svg(
    ~attrs=
      Attr.[
        create("viewBox", "0 0 1 1"),
        create("preserveAspectRatio", "none"),
        create("width", col(1) |> px),
        create("height", row(1) |> px),
        Util.Attrs.style([
          ("position", "absolute"),
          ("left", col(loc.col) |> px),
          ("top", row(loc.row) |> px),
        ]),
      ],
  );
};
