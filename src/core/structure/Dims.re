open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Util;

module Width = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    body: int, // count of last-line chars up to trailing whitespace
    foot: int // count of last-line chars in trailing whitespace
  };
  let mk = (~foot=0, body) => {body, foot};
  let zero = mk(0);
  let total = w => w.body + w.foot;
  let add = (l, r) =>
    l.body == 0 && r.body == 0
      ? mk(0, ~foot=l.foot + r.foot)
      : {body: total(l) + r.body, foot: r.foot};
  // last line indented only if nonempty body
  let indent = (w: t) => {...w, body: w.body + (w.body > 0 ? 2 : 0)};
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  height: int, // number of newlines
  width: Width.t // number of characters in last line
};

let mk = (~height=0, width) => {height, width};
let zero = mk(Width.zero);

let indent = ({height, width}: t) => {
  height,
  width: (height > 0 ? Width.indent : Fun.id)(width),
};

// associative, not commutative
let add = (l: t, r: t) => {
  height: l.height + r.height,
  width: (r.height == 0 ? Width.add(l.width) : Fun.id)(r.width),
};
let sum = List.fold_left(add, zero);

let of_space = (spc: string) => {
  let lines = String.split_on_char('\n', spc);
  let width = Width.mk(0, ~foot=Utf8.length(Lists.ft_exn(lines)));
  mk(~height=List.length(lines) - 1, width);
};

let of_tok = (tok: Token.t) =>
  switch (tok.mtrl) {
  | Space () => of_space(tok.text)
  | Grout(_) => mk(Width.mk(1))
  | Tile(_) => mk(Width.mk(Token.length(tok)))
  };

let rec of_cell = (c: Cell.t): t =>
  switch (Cell.get(c)) {
  | None => zero
  | Some(m) => of_meld(m)
  }
and of_meld = (m: Meld.t) =>
  m
  |> Meld.fold(of_cell, (dims, tok, cell) =>
       sum([
         dims,
         of_tok(tok),
         of_cell(cell) |> (Token.indent(tok) ? indent : Fun.id),
       ])
     );
