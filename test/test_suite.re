open Alcotest;
open Stds;
open Tylr_core;
open Junit_alcotest;

module State = {
  type t = (Path.Cursor.t, string);

  let to_zipper = ((cursor, text): t) => {
    // Use Labeler to get tokens
    let tokens = Labeler.label(text);
    let inserted =
      tokens
      |> Lists.fold_left(
           ~init=Zipper.empty,
           ~f=(zipper, token: Token.Unmolded.t) => {
             // For each token, create an insert edit and apply it
             let edit = Edit.Insert(token.text);
             Edit.perform(edit, zipper) |> Option.get;
           },
         );
    let cell = Zipper.zip(~save_cursor=false, inserted);
    Zipper.unzip_exn(Cell.put_cursor(cursor, cell));
  };
};

let clear_ids = Zipper.map_toks((tok: Token.t) => {...tok, id: 0});

let testable_zipper =
  Alcotest.testable(Fmt.using(Zipper.show, Fmt.string), (a, b) =>
    clear_ids(a) == clear_ids(b)
  );

let check_edit = (init: State.t, edits: list(Edit.t), expected: State.t, ()) => {
  let edited =
    edits
    |> Lists.fold_left(~init=State.to_zipper(init), ~f=(z, a) =>
         Edit.perform(a, z)
         |> Options.get_fail("failed edit " ++ Edit.show(a))
       )
    |> clear_ids;
  let expected = clear_ids(State.to_zipper(expected));
  check(bool, "zipper eq", true, edited == expected);
};

let move_tests = (
  "Move",
  [
    test_case(
      "move left clears selection and leaves cursor left",
      `Quick,
      check_edit(
        (Path.Cursor.select(Selection.mk(~focus=R, ([0], [2]))), "x"),
        [Edit.Move(Step(H(L)))],
        (Path.Cursor.point(Caret.focus([0])), "x"),
      ),
    ),
    test_case(
      "move up clears selection and leaves cursor left",
      `Quick,
      check_edit(
        (Path.Cursor.select(Selection.mk(~focus=R, ([0], [2]))), "x"),
        [Edit.Move(Step(V(L)))],
        (Path.Cursor.point(Caret.focus([0])), "x"),
      ),
    ),
  ],
);

let tab_tests = (
  "Tab",
  [
    test_case(
      "tabbing from const delim ( to ghost )",
      `Quick,
      check_edit(
        (Path.Cursor.point(Caret.focus([2, 0])), "(x"),
        [Edit.Tab(R)],
        (Path.Cursor.point(Caret.focus([2, 2])), "(x"),
      ),
    ),
    test_case(
      "tabbing from const delim in to following hole",
      `Quick,
      check_edit(
        (Path.Cursor.point(Caret.focus([6, 0, 0])), "let = in"),
        [Edit.Tab(R)],
        (Path.Cursor.point(Caret.focus([6, 0, 2])), "let = in"),
      ),
    ),
  ],
);

let insert_tests = (
  "Insert",
  {
    let parse = str => Modify.insert(str, Zipper.empty);
    let parse_by_character = str => {
      print_endline("Str: " ++ str);
      str
      |> String.to_seq
      |> Seq.fold_left(
           (z, c) => {
             print_endline(
               "\nInserting: " ++ String.of_seq(List.to_seq([c])),
             );
             Modify.insert(String.of_seq(List.to_seq([c])), z);
           },
           Zipper.empty,
           _,
         );
    };
    let arb_nonempty_printable_string =
      QCheck.(
        QCheck.add_shrink_invariant(
          s => String.length(s) > 0,
          string_printable_of_size(Gen.int_range(1, 3)),
        )
      );

    [
      QCheck_alcotest.to_alcotest(
        QCheck.Test.make(
          ~name="Insert does not crash",
          ~count=1000,
          arb_nonempty_printable_string,
          str => {
            ignore(parse_by_character(str));
            true;
          },
        ),
      ),
      QCheck_alcotest.to_alcotest(
        QCheck.Test.make(
          ~name="Insert is deterministic",
          ~count=1000,
          arb_nonempty_printable_string,
          str => {
          Alcotest.equal(testable_zipper, parse(str), parse(str))
        }),
      ),
      QCheck_alcotest.to_alcotest(
        QCheck.Test.make(
          ~name="Insert by character is equivalent to Insert",
          ~count=1000,
          arb_nonempty_printable_string,
          str => {
            let parsed = parse(str);
            let parsed_by_character = parse_by_character(str);
            ignore(check(testable_zipper, str, parsed, parsed_by_character));
            true;
          },
        ),
      ),
    ];
  },
);

let modify_tests = (
  "Modify",
  [
    // originally written when tuples required parens, no longer relevant
    // test_case(
    //   "consecutive unmolded tokens get molded once the proper left hand context is inserted",
    //   `Quick,
    //   check_edit(
    //     (Path.Cursor.point(Caret.focus([2, 0, 2])), "let a,b)"),
    //     [Edit.Insert("(")],
    //     (Path.Cursor.point(Caret.focus([2, 2, 0])), "let (a,b)"),
    //   ),
    // ),
    test_case(
      // originally written when tuples required parens
      "redundant tuple comma gets removed even when right paren is ghost (#125)",
      `Quick,
      check_edit(
        (Path.Cursor.point(Caret.focus([2, 1, 1])), "(,"),
        [Edit.Delete(L)],
        (Path.Cursor.point(Caret.focus([2, 0])), "("),
      ),
    ),
  ],
);

let (suite, _) = {
  run_and_report(
    "tylr",
    [move_tests, tab_tests, modify_tests, insert_tests],
  );
};

Junit.to_file(Junit.make([suite]), "junit_tests.xml");
