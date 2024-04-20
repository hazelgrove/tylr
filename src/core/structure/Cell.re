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
    | Step(Path.Step.t);
};

let get = (cell: t) => {
  open OptUtil.Syntax;
  let cursor =
    cell.marks.cursor
    |> Option.map(
         fun
         | ([], cursor) => Found_cursor.Here(cursor)
         | ([step, ..._], _) => There(step),
       );
  let meld =
    cell.meld
    |> Option.map((M(l, W((toks, cells)), r)) => {
         let n = List.length(toks);
         let l = l |> add_marks(Path.Marks.peel(0, marks));
         let cells =
           cells
           |> List.mapi((i, cell) =>
                cell |> add_marks(Path.Marks.peel(i + 1, marks))
              );
         let r = r |> add_marks(Path.Marks.peel(n, marks));
         Meld.M(l, W((toks, cells)), r);
       });
  (cursor, meld);
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

let rec normalize_cursor = (c: Cell.t) =>
  switch (get(c)) {
  | (None | Some(Here(Point)), _) => c
  | (_, None) => put_cursor(([], Point), c)
  | (Some(Here(Select(_, (l, r)))), _) when l == r =>
    put_cursor((l, Point), c)
  | (Some(Here(Select(d, (l, r)))), Some(m)) =>
    let l =
      switch (l) {
      | [i, j] =>
        switch (Chain.nth(i, Meld.to_chain(meld))) {
        | Loop(_) => l
        | Link(tok) =>
          switch (Token.split(j, tok)) {
          | Ok(_)
          | Error(End(L)) => l
          | Error(End(R)) => [i + 1]
          }
        }
      | _ => l
      };
    let r =
      switch (r) {
      | [i, j] =>
        switch (Chain.nth(i, Meld.to_chain(meld))) {
        | Loop(_) => r
        | Link(tok) =>
          switch (Token.split(j, tok)) {
          | Ok(_)
          | Error(End(R)) => r
          | Error(End(L)) => [i - 1]
          }
        }
      | _ => r
      };
    ();

  | (Some(Step(step)), Some(m)) =>
    let m = Meld.map_cell(normalize_cursor, c);
    put(m);
  };

// switch (c.marks.cursor) {
// | None
// | Some((_, Point)) => c
// | Some((steps, Select(d, (l, r)))) =>
//   switch (c.meld) {
//   | None => put_cursor((steps, Point), c)
//   | Some(m) =>
//   }
// }

module Space = {
  let cursor = mk(~marks=Path.Marks.cursor, ());
  let get = (c: t) =>
    switch (get(c)) {
    | None => Some(Token.Space.empty)
    | Some(m) => Meld.Space.get(m)
    };
};
