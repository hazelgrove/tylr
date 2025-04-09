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

let tasks = [];
let _tasks =
  Data.[
    safe_div,
    t3_transcribe,
    t1_transcribe,
    circle_point_q,
    t2_modify_start,
    line_segment_q,
    t4_transcribe,
    contains_q,
    uncurry_modify,
    uncurry_q,
    fuse_modify,
    fuse_q,
  ];
let size = List.length(tasks);

let editor_defaults =
  [serialize(Zipper.empty)]
  @ List.map(
      fun
      | Data.Sexp(sexp) => sexp
      | Text(task) =>
        serialize(parse(Util.Dom.trim_leading_whitespace(task))),
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
