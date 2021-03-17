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

let held_mod_keys = evt => {
  let held = (b, m) => b ? [m] : [];
  let shift = held(Js.to_bool(evt##.shiftKey), Shift);
  let alt = held(Js.to_bool(evt##.altKey), Alt);
  let ctrl = held(Js.to_bool(evt##.ctrlKey), Ctrl);
  let meta = held(Js.to_bool(evt##.metaKey), Meta);
  List.concat([shift, alt, ctrl, meta]);
};

let no_ctrl_alt_meta = evt => {
  let held = m => List.mem(m, held_mod_keys(evt));
  !(held(Ctrl) || held(Alt) || held(Meta));
};

let held_shift = evt => List.mem(Shift, held_mod_keys(evt));

let get_key = evt =>
  Js.to_string(Js.Optdef.get(evt##.key, () => failwith("JsUtil.get_key")));
