open Util;

include Meld.Cell;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Meld.Cell.t(Meld.t);

// let empty = mk();
let is_empty = c => Option.is_none(c.meld);

let put_cursor = (cur: Cursor.t(Path.Point.t, Path.Select.t), cell: t) => {
  ...cell,
  marks: Path.Marks.put_cursor(cur, cell.marks),
};

let face = (~side: Dir.t, c: t) => Option.map(Meld.face(~side), c.meld);

// let has_space = (cell: t) =>
//   switch (cell.meld) {
//   | Some(M(_, W(([tok], [])), _)) when Token.Space.is(tok) => true
//   | _ => false
//   };

let map_marks = (f, cell) => {...cell, marks: f(cell.marks)};
let add_marks = marks => map_marks(Path.Marks.union(marks));
let clear_marks = cell => {...cell, marks: Path.Marks.empty};

module Found_cursor = {
  type t =
    // at the current cell
    | Here(Cursor.t(Path.Point.t, Path.Select.t))
    // in a sub-cell of the current cell
    | There(Path.Step.t);
};

let get_cur = (c: t) => {
  let fc =
    c.marks.cursor
    |> Option.map(
         fun
         | Cursor.Point(p: Path.Point.t) =>
           switch (p.path) {
           | [] => Found_cursor.Here(Point(p))
           | [hd, ..._] => hd mod 2 == 0 ? There(hd) : Here(Point(p))
           }
         | Select(sel: Path.Select.t) =>
           switch (sel) {
           | {focus: [hd_foc, ..._], anchor: [hd_anc, ..._]}
               when hd_foc == hd_anc =>
             There(hd_foc)
           | _ => Here(Select(sel))
           },
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
let get = c => snd(get_cur(c));

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

let rec end_path = (~side: Dir.t, c: t) =>
  switch (get(c)) {
  | None => []
  | Some(M(l, _, r) as m) =>
    let hd = Dir.pick(side, (0, Meld.length(m) - 1));
    let tl = end_path(~side, Dir.pick(side, (l, r)));
    [hd, ...tl];
  };

let point = (~foc=true, ()) => mk(~marks=Path.Marks.point(foc), ());
let is_point = c =>
  if (c == point()) {
    Some(true);
  } else if (c == point(~foc=false, ())) {
    Some(false);
  } else {
    None;
  };

module Space = {
  let get = (c: t) =>
    switch (get(c)) {
    | None => Some(Token.Space.empty)
    | Some(m) => Meld.Space.get(m)
    };
  let is_space = c => Option.is_some(get(c));

  let merge = (l: t, r: t) => {
    open Options.Syntax;
    let+ l_spc = get(l)
    and+ r_spc = get(r);
    let spc = Token.merge(l_spc, r_spc);
    if (Token.is_empty(spc)) {
      {meld: None, marks: Path.Marks.union(l.marks, r.marks)};
    } else {
      {
        meld: Some(Meld.of_tok(spc)),
        marks:
          Path.Marks.union(
            l.marks,
            r.marks
            |> Path.Marks.map_paths(
                 fun
                 | []
                 | [0, ..._] => [1, Token.length(l_spc)]
                 | [1] => [1, Token.length(l_spc)]
                 | [1, j, ..._] => [1, Token.length(l_spc) + j]
                 | path => path,
               ),
          ),
      };
    };
  };

  let get_cur = (c: t) => {
    let (cur, m) = get_cur(c);
    switch (m) {
    | None => Some((cur, Token.Space.empty))
    | Some(m) => Meld.Space.get(m) |> Option.map(spc => (cur, spc))
    };
  };
};
