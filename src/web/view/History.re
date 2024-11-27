open Virtual_dom.Vdom;
open Node;

let is_exn = res =>
  try(String.sub(res, 0, 7) == "Failure") {
  | _ => false
  };
let is_failed = res =>
  try(String.sub(res, 0, 7) == "FailedT") {
  | _ => false
  };

let classify = res =>
  if (is_exn(res)) {
    ("!", ["error"]);
  } else if (is_failed(res)) {
    ("𐄂", ["fail"]);
  } else if (res == "✔") {
    ("✔", []);
  } else {
    ("?", []);
  };

let max_items = 8;

let view = ({hist, _}: Model.t) =>
  label([
    input(~attrs=[Attr.id("history-toggle"), Attr.type_("checkbox")], ()),
    div(
      ~attrs=[Attr.id("history")],
      List.map(
        ((act, res)) => {
          let (sym, cls) = classify(res);
          div(
            ~attrs=[Attr.classes(["history-item"] @ cls)],
            [Node.div([Node.text(act)]), Node.div([Node.text(sym)])],
          );
        },
        List.length(hist) > max_items
          ? Stds.Lists.split_n(hist, max_items) |> fst : hist,
      ),
    ),
  ]);
