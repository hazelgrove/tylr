open Util;

include Meld.Cell;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Meld.Cell.t(Meld.t);

// let empty = mk();
let is_empty = c => Option.is_none(c.meld);

let put_cursor = (path, cell: t) => {
  ...cell,
  marks: Path.Marks.put_cursor(path, cell.marks),
};

let face = (~side: Dir.t, c: t) =>
  switch (c.meld) {
  | None => Space.Molded.t
  | Some(m) => Meld.face(~side, m)
  };

// let has_space = (cell: t) =>
//   switch (cell.meld) {
//   | Some(M(_, W(([tok], [])), _)) when Token.is_space(tok) => true
//   | _ => false
//   };

let add_marks = (marks, cell) => {
  ...cell,
  marks: Path.Marks.union(marks, cell.marks),
};
let clear_marks = cell => {...cell, marks: Path.Marks.empty};

module Found_cursor = {
  type t =
    | Here(Path.Cursor.t)
    | There(Path.Step.t);
};

let get = (c: t) => {
  let fc =
    c.marks.focus
    |> Option.map(
         fun
         | ([], cursor) => Found_cursor.Here(cursor)
         | ([step, ..._], _) => There(step),
       );
  let meld =
    c.meld
    |> Option.map((Meld.M(l, W((toks, cells)), r)) => {
         let n = List.length(toks);
         let l = l |> add_marks(Path.Marks.peel(0, c.marks));
         let cells =
           cells
           |> List.mapi((i, cell) =>
                cell |> add_marks(Path.Marks.peel(i + 1, c.marks))
              );
         let r = r |> add_marks(Path.Marks.peel(n, c.marks));
         Meld.M(l, W((toks, cells)), r);
       });
  (fc, meld);
};

// let empty = () => mk();
let put = (m: Meld.t) =>
  if (Meld.is_empty(m)) {
    empty;
  } else {
    let M(l, W((toks, cells)), r) = m;
    let n = List.length(toks);
    let marks = {
      open Path.Marks;
      let l = cons(0, l.marks);
      let mid = cells |> List.mapi((i, cell) => cons(i + 1, cell.marks));
      let r = cons(n, r.marks);
      union_all([l, ...mid] @ [r]);
    };
    mk(~marks, ~meld=Meld.map_cells(clear_marks, m), ());
  };

// let rec normalize_cursor = (c: t) =>
//   switch (get(c)) {
//   | (None | Some(Here(Point(_))), _) => c
//   | (_, None) => put_cursor(([], Point(true)), c)
//   | (Some(Here(Select(_, (l, r)))), _) when l == r =>
//     put_cursor((l, Point(true)), c)
//   | (Some(Here(Select(d, (l, r)))), Some(m)) =>
//     let l =
//       switch (l) {
//       | [i, j] =>
//         switch (Chain.nth(i, Meld.to_chain(m))) {
//         | Loop(_) => l
//         | Link(tok) =>
//           switch (Token.split(j, tok)) {
//           | Ok(_)
//           | Error(L) => l
//           | Error(R) => [i + 1]
//           }
//         }
//       | _ => l
//       };
//     let r =
//       switch (r) {
//       | [i, j] =>
//         switch (Chain.nth(i, Meld.to_chain(m))) {
//         | Loop(_) => r
//         | Link(tok) =>
//           switch (Token.split(j, tok)) {
//           | Ok(_)
//           | Error(R) => r
//           | Error(L) => [i - 1]
//           }
//         }
//       | _ => r
//       };
//     failwith("todo");

//   | (Some(There(step)), Some(m)) =>
//     let m = Meld.map_cell(normalize_cursor, c);
//     put(m);
//   };

module Space = {
  let point = (~foc=true, ()) => mk(~marks=Path.Marks.point(foc), ());
  let get = (c: t) =>
    switch (get(c)) {
    | (_, None) => Some(Token.Space.empty)
    | (_, Some(m)) => Meld.Space.get(m)
    };
};
