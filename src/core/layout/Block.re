open Stds;
open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

module Line = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(Token.t);
  let nil = [];
  let cat = (@);
  let is_space: t => bool = List.for_all(Token.Space.is);
  let len = toks => toks |> List.map(Token.length) |> List.fold_left((+), 0);
};

module Section = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('block) =
    | Line(Line.t)
    | Block('block);

  let map = (~line, ~block) =>
    fun
    | Line(l) => Line(line(l))
    | Block(b) => Block(block(b));
};

// newline-separated sections, where each newline specifies the indentation of the
// next section relative to the block container. eg [A] 0 [B] 2 [C] 2 [D] looks like
// A
// B
//   C
//   D
[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | B(Chain.t(Section.t(t), int));

let get = (f, B(b)) => f(b);
let put = (f, x) => B(f(x));
let map = (f, B(b)) => B(f(b));

let sec = put(Chain.unit);
let line = l => sec(Line(l));
let wrap = (b: t) => sec(Block(b));

let nil = line(Line.nil);
let cons = (sec: Section.t(t), ~indent=0) => Chain.link(sec, indent);

let height = (B((_, newlines)): t) => List.length(newlines);

let rec sort = (B(b): t): Mtrl.Sorted.t =>
  switch (Chain.hd(b)) {
  | Line([]) => Mtrl.Space()
  | Line([tok, ..._]) => Token.sort(tok)
  | Block(b) => sort(b)
  };
let rec mtrl = (B(b): t): Mtrl.T.t =>
  switch (Chain.hd(b)) {
  | Line([]) => Mtrl.Space(White(Sys))
  | Line([tok, ..._]) => tok.mtrl
  | Block(b) => mtrl(b)
  };

let rec len = (B(b): t) =>
  b |> Chain.to_list(len_sec, Fun.const(1)) |> List.fold_left((+), 0)
and len_sec =
  fun
  | Line(l) => Line.len(l)
  | Block(b) => len(b);

let rec cat_line = (~side: Dir.t, ~line: Line.t, b: t): t => {
  let map_end = f => map(Dir.pick(side, Chain.(map_hd, map_ft), f));
  let cat = l => Dir.pick(side, Line.(cat(line, l), cat(l, line)));
  map_end(Section.map(~line=cat, ~block=cat_line(~side, ~line)), b);
};

let rec hcat = (B(l): t, B(r): t) => B(Chain.cat(hcat_sec, l, r))
and hcat_sec = (l: Section.t(t), r: Section.t(t)) =>
  switch (l, r) {
  | (Line(l), Line(r)) => Line(Line.cat(l, r))
  | (Line(l), Block(r)) => Block(cat_line(~side=L, ~line=l, r))
  | (Block(l), Line(r)) => Block(cat_line(l, ~line=r, ~side=R))
  | (Block(l), Block(r)) => Block(hcat(l, r))
  };
let hcats = (bs: list(t)) => List.fold_right(hcat, bs, nil);

let vcat = (B(l): t, ~indent=0, B(r): t) => B(Chain.append(l, indent, r));
let vcats = bs =>
  switch (Lists.Framed.ft(bs)) {
  | None => raise(Invalid_argument("Block.vcats"))
  | Some((pre, ft)) => List.fold_left((r, l) => vcat(l, r), ft, pre)
  };

let rec is_space = (B(b): t) => List.for_all(is_space_sec, Chain.loops(b))
and is_space_sec =
  fun
  | Section.Line(l) => Line.is_space(l)
  | Block(b) => is_space(b);

// blocks carry indentation info for all but the first section
let nest_tl = (n: int) => map(Chain.map_link((+)(n)));
let unnest_ft = (n: int, B(b): t): (t, bool) =>
  b
  |> Chain.fold_right_map(
       (sec, ind, done_) => (sec, ind - (done_ ? 0 : n), true),
       sec => (sec, !is_space_sec(sec)),
     )
  |> Tuples.map_fst(b => B(b));
let nest_body = (n: int, b: t) => b |> nest_tl(n) |> unnest_ft(n) |> fst;

let of_tok = (tok: Token.t) =>
  switch (tok.mtrl) {
  | Grout(_)
  | Tile(_) => line([tok])
  | Space(_) =>
    Strings.split(~on='\n', tok.text)
    |> List.map(text => line([{...tok, text}]))
    |> vcats
  };

let rec flatten = (B(b): t) =>
  b
  |> Chain.map_loop(flatten_sec)
  |> Chain.fold_left(Fun.id, (acc, ind, b) =>
       vcat(acc, ~indent=ind, nest_tl(ind, b))
     )
and flatten_sec =
  fun
  | Section.Line(l) => sec(Line(l))
  | Block(b) => flatten(b);
let flatten = (b: t) => {
  let B(b) = flatten(b);
  b
  |> Chain.map_loop(
       fun
       | Section.Line(l) => l
       | _ => failwith("bug in flatten"),
     );
};

let nth_line = (b: t, r: Loc.Row.t) => {
  let (lines, inds) = flatten(b);
  let l = List.nth(lines, r);
  let ind = r == 0 ? 0 : List.nth(inds, r - 1);
  (ind, l);
};
