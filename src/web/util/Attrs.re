open Virtual_dom.Vdom.Attr;

// ideally would use css_gen for this but it only accepts int for px units
let style = (kvs: list((string, string))) =>
  create(
    "style",
    kvs |> List.map(((k, v)) => k ++ ":" ++ v ++ ";") |> String.concat(""),
  );
