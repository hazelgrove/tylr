open Virtual_dom.Vdom;

let all =
  Util.Nodes.svg(
    ~attrs=[Attr.id("filters")],
    [
      Silhouette.blur_filter,
      ...List.map(Token.drop_shadow, Tylr_core.Sort.all),
    ],
  );
