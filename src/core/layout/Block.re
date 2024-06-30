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

let rec len = (B(b): t) =>
  Chain.loops(b) |> List.map(len_sec) |> List.fold_left((+), 0)
and len_sec =
  fun
  | Line(l) => Line.len(l)
  | Block(b) => len(b);

let rec cat_line = (~side: Dir.t, ~line: Line.t): (t => t) => {
  let map_end = f => map(Dir.pick(side, Chain.(map_hd, map_ft), f));
  let cat = l => Dir.pick(side, Line.(cat(line, l), cat(l, line)));
  map_end(Section.map(~line=cat, ~block=cat_line(~side, ~line)));
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
  | Space () =>
    Strings.split(~on='\n', tok.text)
    |> List.map(text => line([{...tok, text}]))
    |> vcats
  };
