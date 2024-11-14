open Tylr_core;

/* given base program string, figure out how many copies to concatenate
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
  Util.TimeUtil.get_time(() => progam_str |> Store.parse |> ignore);

let time_per_line = (lines: int, time: int): float => {
  float_of_int(time) /. float_of_int(lines);
};

let benchmark_parsing = () => {
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

let cursor_depth = (z: Zipper.t) => {
  let cell = Zipper.zip(~save_cursor=true, z);
  switch (cell.marks.cursor) {
  | Some(Point(cursor)) => cursor.path |> List.length
  | _ => failwith("benchmark: no cursor")
  };
};

let benchmark_hole_fills = () => {
  let program_str = Data.holey;
  let reps_per_action = 200;
  let zipper_action = Modify.insert("X");

  print_endline("BENCHMARK: filling hole edits at different depths");
  let a = benchmark(program_str);
  print_endline("BENCHMARK: warmup parse: " ++ string_of_int(a) ++ "ms");
  let b = benchmark(program_str);
  print_endline(
    "BENCHMARK: same parse after warmup: " ++ string_of_int(b) ++ "ms",
  );
  let z =
    List.fold_left(
      (z_acc, _x) => {
        // perform insertion action repeatedly
        let t =
          Util.TimeUtil.get_time(() => {
            for (_ in 1 to reps_per_action) {
              let _ = zipper_action(z_acc);
              ();
            }
          });
        print_endline(
          "depth: "
          ++ string_of_int(cursor_depth(z_acc))
          ++ ", "
          ++ "time for "
          ++ string_of_int(reps_per_action)
          ++ " insertions:"
          ++ string_of_int(t)
          ++ "ms",
        );
        // fill hole and return zipper with caret moved to previous hole
        let z2 = Modify.insert("X", z_acc);
        switch (Tab.perform(L, z2)) {
        | None => z2
        | Some(z3) => z3
        };
      },
      // starts at bottom of holey program
      Store.parse(program_str),
      List.init(20, _ => ()),
    );
  ();
  print_endline("program after actions:");
  print_endline(z |> Zipper.to_string);
};

//benchmark_parsing();
benchmark_hole_fills();
