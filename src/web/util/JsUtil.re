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

let date_now = () => {
  %js
  new Js.date_now;
};

let timestamp = () => date_now()##valueOf;

[@deriving show({with_path: false})]
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

[@deriving show({with_path: false})]
type key = string;

[@deriving show({with_path: false})]
type modifiers = list(mod_key);

[@deriving show({with_path: false})]
type key_dir =
  | KeyUp
  | KeyDown;

[@deriving show({with_path: false})]
type key_event = {
  dir: key_dir,
  key,
  modifiers,
  is_mac: bool,
};

let get_key = evt =>
  Js.to_string(Js.Optdef.get(evt##.key, () => failwith("JsUtil.get_key")));

let mk_key_event = (dir, evt): key_event => {
  let key = get_key(evt);
  let is_mac = Os.is_mac^;
  let modifiers =
    (held(Ctrl, evt) ? [Ctrl] : [])
    @ (held(Shift, evt) ? [Shift] : [])
    @ (held(Alt, evt) ? [Alt] : [])
    @ (held(Meta, evt) ? [Meta] : []);
  {dir, key, modifiers, is_mac};
};
