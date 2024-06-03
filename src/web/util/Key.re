open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Js_of_ocaml;

[@deriving (show({with_path: false}), yojson)]
type dir =
  | KeyUp
  | KeyDown;

[@deriving (show({with_path: false}), yojson)]
type key =
  | D(string)
  | U(string);

[@deriving (show({with_path: false}), yojson)]
type sys =
  | Mac
  | PC;

[@deriving (show({with_path: false}), yojson)]
type held =
  | Down
  | Up;

[@deriving (show({with_path: false}), yojson)]
type t = {
  key,
  sys,
  shift: held,
  meta: held,
  ctrl: held,
  alt: held,
};

let key_of = (dir: dir, evt): key => {
  let key =
    Js.to_string(Js.Optdef.get(evt##.key, () => failwith("Key.key_of")));
  switch (dir) {
  | KeyUp => U(key)
  | KeyDown => D(key)
  };
};

let to_held: bool => held = b => b ? Down : Up;

let mk = (dir: dir, evt): t => {
  key: key_of(dir, evt),
  sys: Os.is_mac^ ? Mac : PC,
  shift: to_held(Js.to_bool(evt##.shiftKey)),
  meta: to_held(Js.to_bool(evt##.metaKey)),
  ctrl: to_held(Js.to_bool(evt##.ctrlKey)),
  alt: to_held(Js.to_bool(evt##.altKey)),
};

let modifier_string = (h: held, m): string => h == Down ? " + " ++ m : "";

let modifiers_string = (key: t): string =>
  modifier_string(key.shift, "SHIFT")
  ++ modifier_string(key.meta, "META")
  ++ modifier_string(key.ctrl, "CTRL")
  ++ modifier_string(key.alt, "ALT");

let key_dir_string = (key: t): string =>
  switch (key.key) {
  | U(key) => "(UP): " ++ key
  | D(key) => "(DN): " ++ key
  };

let to_string = (key: t): string =>
  "KEY" ++ key_dir_string(key) ++ modifiers_string(key);

let is_printable = s => Re.Str.(string_match(regexp("^[ -~]$"), s, 0));
let is_digit = s => Re.Str.(string_match(regexp("^[0-9]$"), s, 0));
let is_f_key = s => Re.Str.(string_match(regexp("^F[0-9][0-9]*$"), s, 0));
