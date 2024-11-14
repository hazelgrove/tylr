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

let tasks = []; //Data.longlong,
//Data.epzz,
// Data.epz0,
// Data.epz1,
// Data.epz2,
// Data.epz3,
// Data.epz4,
//Data.t0_transcribe,
// Data.t1_transcribe,
// Data.t2_transcribe,
// Data.t3_transcribe,
// Data.t4_transcribe,
// Data.emoji_paint,
// Data.t0_modify,
// Data.t1_modify,
// Data.t2_modify,
// Data.t3_modify,
// (("case 7\n| x => 7")),
// (("let (a, b) =\n(8*9<6, 17==6) in\n(a,(a, b))")),

// (("let f = fun z -> 9 in f(9)")),

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

/* given base program string Data.longlong, figure out how many copies to concatenate
   to get a program of the provided number of lines, do that concat, and then trim to the exact line count.
    */
let get_program_string_of_length = (program_str, num_lines: int): string => {
  let base_lines = program_str |> String.split_on_char('\n');
  let base_num_lines = base_lines |> List.length;
  let num_copies = num_lines / base_num_lines;
  let remainder = num_lines mod base_num_lines + 1;
  let program = String.concat("\n", List.init(num_copies, _ => program_str));
  let program =
    program
    ++ String.concat("\n", fst(Stds.Lists.split_n(base_lines, remainder)));
  program;
};

let benchmark = progam_str =>
  Util.TimeUtil.get_time(() => progam_str |> parse |> ignore);

let time_per_line = (lines: int, time: int): float => {
  float_of_int(time) /. float_of_int(lines);
};

let suite = () => {
  let program_str = Data.longlong;
  print_endline(
    "BENCHMARK: parsing (prefix of) complete program with no internal obligations",
  );
  let num_tokens = Labeler.label(program_str) |> List.length;
  let num_chars = program_str |> String.length;
  let num_lines = program_str |> String.split_on_char('\n') |> List.length;
  print_endline(
    "BENCHMARK: base program consists of "
    ++ string_of_int(num_lines)
    ++ " lines, "
    ++ string_of_int(num_chars)
    ++ " chars, and "
    ++ string_of_int(num_tokens)
    ++ " tokens",
  );
  print_endline(
    "BENCHMARK: Average tokens per line: "
    ++ string_of_float(float_of_int(num_tokens) /. float_of_int(num_lines)),
  );
  print_endline(
    "BENCHMARK: Average chars per line: "
    ++ string_of_float(float_of_int(num_chars) /. float_of_int(num_lines)),
  );
  let a = benchmark(program_str);
  print_endline("BENCHMARK: warmup parse: " ++ string_of_int(a) ++ "ms");
  let b = benchmark(program_str);
  print_endline(
    "BENCHMARK: same parse after warmup: " ++ string_of_int(b) ++ "ms",
  );
  for (i in 1 to 20) {
    let num_lines = 30 * i;
    let program_str = get_program_string_of_length(program_str, num_lines);
    let c = benchmark(program_str);
    print_endline(
      "ITERATION "
      ++ string_of_int(i)
      ++ " :"
      ++ string_of_int(num_lines)
      ++ " lines. parsed in "
      ++ string_of_int(c)
      ++ "ms",
    );
    // print_endline(
    //   "time per line: "
    //   ++ string_of_float(time_per_line(num_lines, c))
    //   ++ "ms",
    // );
  };
};

suite();
