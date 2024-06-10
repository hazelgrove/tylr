open Virtual_dom.Vdom;

let px = f => Printf.sprintf("%fpx", f);

let mk = (~font, ~pos: Tylr_core.Layout.Pos.t, ~width, ~height) => {
  let (row, col) = Model.Font.(row(font), col(font));
  Util.Nodes.svg(
    ~attrs=
      Attr.[
        create("viewBox", Printf.sprintf("0 0 %d %d", width, height)),
        create("preserveAspectRatio", "none"),
        create("width", col(width) |> px),
        create("height", row(height) |> px),
        Util.Attrs.style([
          ("left", col(pos.col) |> px),
          ("top", row(pos.row) |> px),
        ]),
      ],
  );
};
