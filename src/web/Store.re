open Tylr_core;

let insert: (Zipper.t, string) => Zipper.t =
  (z, str) => {
    switch (Edit.perform(Insert(str), z)) {
    | None =>
      print_endline("WARNING: Store.insert failed");
      z;
    | Some(r) => r
    };
  };

let parse = insert(Zipper.empty);

let serialize = z => z |> Zipper.sexp_of_t |> Sexplib.Sexp.to_string;

let deserialize = (str: string): Zipper.t =>
  try(str |> Sexplib.Sexp.of_string |> Zipper.t_of_sexp) {
  | _ => failwith("WARNING: Store.deserialize: exception during parse")
  };

let save_syntax_key: int => string =
  save_idx => "SAVE" ++ string_of_int(save_idx);

let save_syntax = (save_idx: int, z: Zipper.t) =>
  LocalStorage.set(save_syntax_key(save_idx), z |> serialize);

let tasks = [
  //Data.epzz,
  // Data.epz0,
  // Data.epz1,
  // Data.epz2,
  // Data.epz3,
  // Data.epz4,
  //Data.t0_transcribe,
  Data.t1_transcribe,
  Data.t2_transcribe,
  Data.t3_transcribe,
  Data.t4_transcribe,
  // Data.emoji_paint,
  // Data.t0_modify,
  // Data.t1_modify,
  // Data.t2_modify,
  // Data.t3_modify,
  // (("case 7\n| x => 7")),
  // (("let (a, b) =\n(8*9<6, 17==6) in\n(a,(a, b))")),
  // (("let f = fun z -> 9 in f(9)")),
];

let editor_defaults =
  [serialize(Zipper.empty)]
  @ List.map(
      task => serialize(parse(Util.Dom.trim_leading_whitespace(task))),
      tasks,
    );

let load_default_syntax: int => Zipper.t =
  save_idx =>
    switch (List.nth_opt(editor_defaults, save_idx)) {
    | None => Zipper.empty
    | Some(str) => deserialize(str)
    };

let load_syntax: int => Zipper.t =
  save_idx =>
    switch (LocalStorage.get(save_syntax_key(save_idx))) {
    | None => load_default_syntax(save_idx)
    | Some(str) => deserialize(str)
    };

//let unparse: Zipper.t => string = z => z |> Zipper.zip |> Cell.tokens;
