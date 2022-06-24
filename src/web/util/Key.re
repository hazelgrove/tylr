open Js_of_ocaml;

[@deriving show({with_path: false})]
type mod_key =
  | Shift
  | Alt
  | Ctrl
  | Meta;

[@deriving show({with_path: false})]
type key = string;

[@deriving show({with_path: false})]
type dir =
  | KeyUp
  | KeyDown;

[@deriving show({with_path: false})]
type key_and_dir =
  | D(key)
  | U(key);

[@deriving show({with_path: false})]
type platform =
  | IsMac
  | IsPC;

[@deriving show({with_path: false})]
type held =
  | Down
  | Up;

[@deriving show({with_path: false})]
type ctrl_held =
  | Ctrl(held);

[@deriving show({with_path: false})]
type shift_held =
  | Shift(held);

[@deriving show({with_path: false})]
type alt_held =
  | Alt(held);

[@deriving show({with_path: false})]
type meta_held =
  | Meta(held);

[@deriving show({with_path: false})]
type t = (key_and_dir, platform, shift_held, meta_held, ctrl_held, alt_held);

let get_key = evt =>
  Js.to_string(Js.Optdef.get(evt##.key, () => failwith("JsUtil.get_key")));

let modifiers = evt =>
  (Js.to_bool(evt##.ctrlKey) ? [(Ctrl: mod_key)] : [])
  @ (Js.to_bool(evt##.shiftKey) ? [(Shift: mod_key)] : [])
  @ (Js.to_bool(evt##.altKey) ? [(Alt: mod_key)] : [])
  @ (Js.to_bool(evt##.metaKey) ? [(Meta: mod_key)] : []);

let held = (modifier: mod_key, evt) =>
  List.mem(modifier: mod_key, modifiers(evt)) ? Down : Up;

let mk = (dir, evt): t => {
  let platform = Os.is_mac^ ? IsMac : IsPC;
  let key_and_dir =
    switch (dir) {
    | KeyUp => U(get_key(evt))
    | KeyDown => D(get_key(evt))
    };
  (
    key_and_dir,
    platform,
    Shift(held(Shift, evt)),
    Meta(held(Meta, evt)),
    Ctrl(held(Ctrl, evt)),
    Alt(held(Alt, evt)),
  );
};
