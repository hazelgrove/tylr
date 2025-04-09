open Alcotest;
open Stds;
open Tylr_core;

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

let () = {
  run("tylr", [move_tests, tab_tests, modify_tests]);
};
