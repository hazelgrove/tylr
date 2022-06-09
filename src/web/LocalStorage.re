open Js_of_ocaml;
open Core;

let editor_defaults = [
  "0",
  "let foo =
fun taz => {
case taz of {
| (2, torb) => bargle + 7*torb
| (blee, 5) => krunk ? blee : 66
}
}
in foo(0!)",
  "let foo = fun taz => (fun bar => (taz + 2*bar)) in foo(1!)",
  "3",
  "4",
  "5",
  "6",
  "7",
  "8",
  "9",
];

let get_localstore = (k: string): option(string) =>
  try({
    let local_store =
      Js.Optdef.get(Dom_html.window##.localStorage, () => assert(false));
    local_store##getItem(Js.string(k))
    |> (
      x => Js.Opt.get(x, () => assert(false)) |> Js.to_string |> Option.some
    );
  }) {
  | _ => None
  };

let set_localstore = (k: string, v: string): unit => {
  let local_store =
    Js.Optdef.get(Dom_html.window##.localStorage, () => assert(false));
  local_store##setItem(Js.string(k), Js.string(v));
};

let insert': ((Zipper.t, IdGen.state), string) => (Zipper.t, IdGen.state) =
  (z_id, c) => {
    switch (
      Zipper.perform(Insert(c == "\n" ? Whitespace.linebreak : c), z_id)
    ) {
    | Error(err) =>
      print_endline(Zipper.Action.Failure.show(err));
      z_id;
    | Ok(r) => r
    };
  };

let parse': (IdGen.state, string) => (Zipper.t, IdGen.state) =
  (id_gen, s) =>
    s
    |> Util.StringUtil.to_list
    |> List.fold_left(insert', (Model.empty_zipper, id_gen));

let save_key = save_idx => "SAVE" ++ string_of_int(save_idx);

let save_to_local_text = (save_idx: int, z: Zipper.t) =>
  set_localstore(
    save_key(save_idx),
    z |> Zipper.zip |> Code.CodeString.of_segment,
  );

let text_to_zid_opt =
    (id_gen: IdGen.state, str: string): option((Zipper.t, IdGen.state)) =>
  try(Some(parse'(id_gen, str))) {
  | _ =>
    print_endline("ERROR: text_to_zid_opt: exception during parse");
    None;
  };

let load_default: (int, IdGen.state) => option((Zipper.t, IdGen.state)) =
  (save_idx, id_gen) =>
    switch (List.nth_opt(editor_defaults, save_idx)) {
    | Some(str) => text_to_zid_opt(id_gen, str)
    | None => None
    };

let load_from_local_text:
  (int, IdGen.state) => option((Zipper.t, IdGen.state)) =
  (save_idx, id_gen) =>
    switch (get_localstore(save_key(save_idx))) {
    | None => load_default(save_idx, id_gen)
    | Some(str) => text_to_zid_opt(id_gen, str)
    };
