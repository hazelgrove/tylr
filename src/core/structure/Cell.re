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
let pop_marks = (n, cell) => (
  Path.Marks.cons(n, cell.marks),
  clear_marks(cell),
);

let aggregate_marks = (c: t) =>
  switch (c.meld) {
  | None => c
  | Some(M(l, W((ts, cs)), r) as m) =>
    open Path.Marks;
    let (l_marks, l) = pop_marks(0, l);
    let (ts_marks, ts) =
      ts |> List.mapi(i => Token.pop_marks(1 + 2 * i)) |> List.split;
    let (cs_marks, cs) =
      cs |> List.mapi(i => pop_marks(2 * (1 + i))) |> List.split;
    let (r_marks, r) = pop_marks(Meld.length(m) - 1, r);
    let marks =
      union_all(List.concat([[l_marks], ts_marks, cs_marks, [r_marks]]));
    {marks, meld: Some(M(l, W((ts, cs)), r))};
  };

let distribute_marks = (c: t) =>
  switch (c.meld) {
  | None => c
  | Some(M(l, W((ts, cs)), r) as m) =>
    let l = l |> add_marks(Path.Marks.peel(0, c.marks));
    let ts =
      ts
      |> List.mapi((i, t) => {
           let marks = Path.Marks.peel(1 + 2 * i, c.marks);
           Token.(add_marks(Marks.of_paths(marks), t));
         });
    let cs =
      cs |> List.mapi(i => add_marks(Path.Marks.peel(2 * (1 + i), c.marks)));
    let r = r |> add_marks(Path.Marks.peel(Meld.length(m) - 1, c.marks));
    mk(~meld=Meld.M(l, W((ts, cs)), r), ());
  };

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
  let c = distribute_marks(c);
  (fc, c.meld);
};
let get = c => snd(get_cur(c));

// let empty = () => mk();
let put = (meld: Meld.t) => aggregate_marks(mk(~meld, ()));

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
