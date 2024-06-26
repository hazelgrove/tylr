// open Js_of_ocaml;
  // open Tylr_core;
  // let default_editor_idx = 1;
  // let editor_defaults = [
  //   "",
  //   "",
  //   "fun center, p ->
  // let x1, y1 = center in
  // let x2, y2 = p in
  // let r = sqrt(pow(x1 - x2, 2) + pow(y1 - y2, 2)) in
  // circle(center, r)
  // ",
  //   "",
  //   "shapes
  // |> map(rotate(pi / 4))
  // |> map(translate(6, 7))
  // |> filter(fun shape -> area(shape) < 50)
  // |> map(dilate(5))
  // ",
  //   "",
  //   "fun square, p1, p2 ->
  // if square then
  // let mark =
  // fun center ->
  // let x, y = center in
  // rect(x - 2, y - 2, 4, 4)
  // in
  // [mark(p1); line(p1, p2); mark(p2)]
  // else
  // let mark =
  // fun center ->
  // let r = 4 in
  // circle(center, 4)
  // in
  // [mark(p1); line(p1, p2); mark(p2)]
  // ",
  //   "let ss1 = observe(msg, map_rotate(map_dilate(shapes))) in
  // let ss2 = map_brighten(shapes) in
  // [ss1; ss2]
  // ",
  //   "let foo =
  // fun taz ->
  // case taz of
  // | (2, torb) -> bargle + 7*torb
  // | (blee, 5) -> krunk ? blee : 66
  // in foo(0!)",
  //   "let foo = fun taz -> (fun bar -> (taz + 2*bar)) in foo(1!)",
  // ];
  // let editor_captions = [
  //   "The zeroth editor. Silent; Serene.",
  //   "A Foo calculator",
  //   "Another method of calculating foos",
  //   "A circle centerer",
  //   "The fourth editor. Blocky; Staward",
  //   "The fifth editor. Hums with numinous portent.",
  //   "The sixth editor. Holds forbidden knowledge",
  //   "The seventh editor. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
  //   "The eigth editor. Spidery.",
  //   "The ninth editor: The final gate.",
  // ];
  // assert(List.length(editor_captions) == List.length(editor_defaults));
  // let num_editors = List.length(editor_defaults);
  // let get_localstore = (k: string): option(string) =>
  //   try({
  //     let local_store =
  //       Js.Optdef.get(Dom_html.window##.localStorage, () => assert(false));
  //     local_store##getItem(Js.string(k))
  //     |> (
  //       x => Js.Opt.get(x, () => assert(false)) |> Js.to_string |> Option.some
  //     );
  //   }) {
  //   | _ => None
  //   };
  // let set_localstore = (k: string, v: string): unit => {
  //   let local_store =
  //     Js.Optdef.get(Dom_html.window##.localStorage, () => assert(false));
  //   local_store##setItem(Js.string(k), Js.string(v));
  // };
  // let save_syntax_key: int => string =
  //   save_idx => "SAVE" ++ string_of_int(save_idx);
  // let save_ed_idx_key: string = "CURRENT_EDITOR";
  // let save_settings_key: string = "SETTINGS";
  // let action_log_key: string = "ACTION_LOG";
  // let keystoke_log_key: string = "KEYSTROKE_LOG";
  // let zipper_log_key: string = "ZIPPER_LOG";
  // let insert_to_zid: (Zipper.state, string) => Zipper.state =
  //   (z_id, c) => {
  //     switch (
  //       Zipper.perform(Insert(c == "\n" ? Whitespace.linebreak : c), z_id)
  //     ) {
  //     | Error(err) =>
  //       print_endline("WARNING: insert: " ++ Zipper.Action.Failure.show(err));
  //       z_id;
  //     | Ok(r) => r
  //     };
  //   };
  // let parse_to_zid = (id_gen: IdGen.state, str: string): option(Zipper.state) =>
  //   try(
  //     str
  //     |> Stds.Strings.to_list
  //     |> List.fold_left(insert_to_zid, (Model.empty_zipper, id_gen))
  //     |> Option.some
  //   ) {
  //   | _ =>
  //     print_endline("WARNING: parse_to_zid: exception during parse");
  //     None;
  //   };
  // let save_syntax = (save_idx: int, z: Zipper.t) =>
  //   set_localstore(
  //     save_syntax_key(save_idx),
  //     z |> Zipper.zip |> Printer.of_segment,
  //   );
  // let load_default_syntax: (int, IdGen.state) => option(Zipper.state) =
  //   (save_idx, id_gen) =>
  //     switch (List.nth_opt(editor_defaults, save_idx)) {
  //     | Some(str) => parse_to_zid(id_gen, str)
  //     | None => None
  //     };
  // let load_syntax: (int, IdGen.state) => option(Zipper.state) =
  //   (save_idx, id_gen) =>
  //     switch (get_localstore(save_syntax_key(save_idx))) {
  //     | None => load_default_syntax(save_idx, id_gen)
  //     | Some(str) => parse_to_zid(id_gen, str)
  //     };
  // let save_editor_idx = (editor_idx: int): unit =>
  //   set_localstore(save_ed_idx_key, string_of_int(editor_idx));
  // let load_editor_idx = (): int =>
  //   switch (get_localstore(save_ed_idx_key)) {
  //   | None => default_editor_idx
  //   | Some(idx) =>
  //     switch (int_of_string_opt(idx)) {
  //     | None => default_editor_idx
  //     | Some(idx) => idx
  //     }
  //   };
  // let save_settings = (settings: Model.settings): unit =>
  //   set_localstore(
  //     save_settings_key,
  //     settings |> Model.sexp_of_settings |> Sexplib.Sexp.to_string,
  //   );
  // let load_settings = (): Model.settings =>
  //   switch (get_localstore(save_settings_key)) {
  //   | None => Model.settings_init
  //   | Some(flag) =>
  //     try(flag |> Sexplib.Sexp.of_string |> Model.settings_of_sexp) {
  //     | _ => Model.settings_init
  //     }
  //   };