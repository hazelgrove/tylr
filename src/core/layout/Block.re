module Line = {
  type t = list(Token.t);
  let nil = [];
  let cat = (@);
};

module Section = {
  type t('block) =
    | Line(Line.t)
    | Block('block);

  let hcat = (~hcat_block, l, r) =>
    switch (l, r) {
    | (Line(l), Line(r)) => Line(Line.cat(l, r))
    | (Line(l), Block(r)) => Block(cat_hd(l, r))
    | (Block(l), Line(r)) => Block(cat_ft(l, r))
    | (Block(l), Block(r)) => Block(hcat_block(l, r))
    };
};

type t =
  | B(Chain.t(Section.t(t), int));

let get = (f, B(b)) => f(b);
let put = (f, x) => B(f(x));

let sec = put(Chain.unit);
let line = l => sec(Line(l));
let wrap = (b: t) => sec(Block(b));

let nil = line(Line.nil);
let cons = (sec: section, ~indent=0) => Chain.link(sec, indent);

let map_hd = get(Chain.map_hd);
let map_ft = get(Chain.map_ft);

let rec cat_hd = (line: Line.t, block: t) =>
  block
  |> map_hd(
       fun
       | Line(l) => Line.cat(line, l)
       | Block(b) => cat_hd(line, b),
     );
let rec cat_ft = (block: t, line: Line.t) =>
  block
  |> map_ft(
       fun
       | Line(l) => Line.cat(l, line)
       | Block(b) => cat_ft(line, b),
     );

let rec hcat = (l: t, r: t) =>
  Chain.cat(Section.hcat(~hcat_block=hcat), l, r);
let hcats = (bs: list(t)) => List.fold_right(hcat, bs, empty);
let vcat = (l: t, ~indent=0, r: t) => Chain.append(l, indent, r);

// blocks carry indentation info for all but the first section
let nest_tl = (n: int) => Chain.map_link((+)(n));
let unnest_ft = (n: int, b: Block.t) =>
  b
  |> Chain.fold_right_map(
       (sec, ind, should_unnest) =>
         (sec, ind - (should_unnest ? n : 0), false),
       sec => (sec, is_space(sec)),
     );
let nest_body = (n: int, b: Block.t) =>
  b |> nest_tl(n) |> unnest_ft(n) |> fst;

let of_tok = (tok: Token.t) =>
  switch (tok.mtrl) {
  | Grout(_)
  | Tile(_) => line([tok])
  | Space =>
    Strings.split('\n', tok.text)
    |> List.map(text => Block.line([{...tok, text}]))
    |> hcats
  };
