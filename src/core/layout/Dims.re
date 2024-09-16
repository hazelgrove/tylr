open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Stds;

// module Width = {
//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type t = {
//     body: int, // count of last-line chars up to trailing whitespace
//     foot: int // count of last-line chars in trailing whitespace
//   };
//   let mk = (~foot=0, body) => {body, foot};
//   let zero = mk(0);
//   let total = w => w.body + w.foot;
//   let add = (l, r) =>
//     l.body == 0 && r.body == 0
//       ? mk(0, ~foot=l.foot + r.foot)
//       : {body: total(l) + r.body, foot: r.foot};
//   // last line indented only if nonempty body
//   let indent = (w: t) => {...w, body: w.body + (w.body > 0 ? 2 : 0)};
// };

// [@deriving (show({with_path: false}), sexp, yojson)]
// type t = {
//   height: int, // number of newlines
//   width: Width.t // number of characters in last line
// };

// let mk = (~height=0, width) => {height, width};
// let zero = mk(Width.zero);

// let indent = ({height, width}: t) => {
//   height,
//   width: (height > 0 ? Width.indent : Fun.id)(width),
// };

// let of_space = (spc: string) => {
//   let lines = String.split_on_char('\n', spc);
//   let width = Width.mk(0, ~foot=Utf8.length(Lists.ft_exn(lines)));
//   mk(~height=List.length(lines) - 1, width);
// };

// let of_tok = (tok: Token.t) =>
//   switch (tok.mtrl) {
//   | Space(_) => of_space(tok.text)
//   | Grout(_) => mk(Width.mk(1))
//   | Tile(_) => mk(Width.mk(Token.length(tok)))
//   };

// let rec of_cell = (c: Cell.t): t =>
//   switch (Cell.get(c)) {
//   | None => zero
//   | Some(m) => of_meld(m)
//   }
// and of_meld = (m: Meld.t) =>
//   m
//   |> Meld.fold(of_cell, (dims, tok, cell) =>
//        sum([
//          dims,
//          of_tok(tok),
//          of_cell(cell) |> (Token.indent(tok) ? indent : Fun.id),
//        ])
//      );

module Width = {
  // metrics describing the first/last line of a block
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    // column count of leading/trailing whitespace
    pad: int,
    // column count of the rest of the line
    rest: int,
  };
  let mk = (~pad=0, rest) => {pad, rest};
  let zero = {pad: 0, rest: 0};
  let total = ({pad, rest}: t) => pad + rest;
  let add_same = (outer: t, inner: t) =>
    outer.rest == 0
      ? {pad: outer.pad + inner.pad, rest: inner.rest}
      : {pad: outer.pad, rest: outer.rest + total(inner)};
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  height: int,
  // if height == 0, then
  // Width.total(fst(widths)) == Width.total(snd(widths))
  widths: (Width.t, Width.t),
};

let zero = {height: 0, widths: Width.(zero, zero)};
let newline = {height: 1, widths: Width.(zero, zero)};
let indent = (n: int, {height, widths: (top, bot)}: t) => {
  height,
  widths: (
    {...top, pad: n + top.pad},
    bot.rest == 0
      ? {...bot, pad: n + bot.pad} : {...bot, rest: n + bot.rest},
  ),
};

// // associative, not commutative
let add = (l: t, r: t) => {
  height: l.height + r.height,
  widths: (
    l.height == 0
      ? Width.add_same(fst(l.widths), fst(r.widths)) : fst(l.widths),
    r.height == 0
      ? Width.add_same(snd(r.widths), snd(l.widths)) : snd(r.widths),
  ),
};
let sum = List.fold_left(add, zero);

let tok = (~height=0, width: Width.t) => {height, widths: (width, width)};

let of_space = (spc: string) => {
  let lines = String.split_on_char('\n', spc);
  let width = Width.mk(0, ~pad=Utf8.length(Lists.ft_exn(lines)));
  tok(~height=List.length(lines) - 1, width);
};

let of_tok = (t: Token.t) => {
  switch (t.mtrl) {
  | Space(_) => of_space(t.text)
  | Grout(_) => tok(Width.mk(1))
  | Tile(_) => tok(Width.mk(Token.length(t)))
  };
};

let rec of_block = (B(b): Block.t) =>
  b
  |> Chain.fold_left_map(
       sec => ((), of_sec(sec)),
       ((), ind, sec) => {
         let sec = of_sec(sec) |> add(newline) |> indent(ind);
         ((), (), sec);
       },
     )
  |> snd
  |> Chain.loops
  |> sum
and of_sec =
  fun
  | Block.Section.Line(line) => sum(List.map(of_tok, line))
  | Block(b) => of_block(b);

let skip = (loc: Loc.t, ~over: t, ~ind: Loc.Col.t) =>
  Loc.{
    row: loc.row + over.height,
    col: (over.height > 0 ? ind : loc.col) + Width.total(snd(over.widths)),
  };
