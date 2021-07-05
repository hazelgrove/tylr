open Js_of_ocaml;

let get_elem_by_id = id => {
  let doc = Dom_html.document;
  Js.Opt.get(
    doc##getElementById(Js.string(id)),
    () => {
      print_endline(id);
      assert(false);
    },
  );
};

type mod_key =
  | Shift
  | Alt
  | Ctrl
  | Meta;

let held = (m: mod_key, evt) =>
  switch (m) {
  | Shift => Js.to_bool(evt##.shiftKey)
  | Alt => Js.to_bool(evt##.altKey)
  | Ctrl => Js.to_bool(evt##.ctrlKey)
  | Meta => Js.to_bool(evt##.metaKey)
  };

let get_key = evt =>
  Js.to_string(Js.Optdef.get(evt##.key, () => failwith("JsUtil.get_key")));

let date_now = () => {
  print_endline("date_now");
  %js
  new Js.date_now;
};
