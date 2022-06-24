[@deriving show({with_path: false})]
type dir =
  | KeyUp
  | KeyDown;

[@deriving show({with_path: false})]
type key =
  | D(string)
  | U(string);

[@deriving show({with_path: false})]
type sys =
  | Mac
  | PC;

[@deriving show({with_path: false})]
type held =
  | Down
  | Up;

[@deriving show({with_path: false})]
type t = {
  key,
  sys,
  shift: held,
  meta: held,
  ctrl: held,
  alt: held,
};

let key_of = (dir: dir, evt): key => {
  let key = JsUtil.get_key(evt);
  switch (dir) {
  | KeyUp => U(key)
  | KeyDown => D(key)
  };
};

let to_held: bool => held = b => b ? Down : Up;

let mk = (dir, evt): t => {
  key: key_of(dir, evt),
  sys: Os.is_mac^ ? Mac : PC,
  shift: to_held(JsUtil.shift_held(evt)),
  meta: to_held(JsUtil.meta_held(evt)),
  ctrl: to_held(JsUtil.ctrl_held(evt)),
  alt: to_held(JsUtil.alt_held(evt)),
};
