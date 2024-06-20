// module Block = {
//   type t('blocks) =
//     | Line({len: int})
//     | Group({indent: int, blocks: 'blocks});
//   let zero = Line(0);
// };
// module Block = {
//   type t = Chain.t(Chunk.t(t), unit);
//   let zero = Chain.unit(Chunk.zero);
// };

module Line = {
  type t = {
    // length of leading whitespace (excluding indentation)
    space: int,
    // length of everything else
    subst: int,
  };
  let is_space = ({subst, _}: t) => subst == 0;
  let len = ({space, subst}: t) => space + subst;
  let cat = (l: t, r: t) => {space: l.space, subst: l.subst + len(r)};
};
module Chunk = {
  type t('block) =
    | Line(Line.t)
    | Block({
        indent: int,
        block: 'block,
      });
  let empty = Line({len: 0});
  let indent = (~block_of_line) =>
    fun
    | Line(line) => Block({indent: 2, block: block_of_line(line)})
    // avoid double-indentation
    | Block({indent: _, block}) => Block({indent: 2, block});
};
module Block = {
  include Chain;
  type t = Chain.t(Chunk.t(t), unit);
  let mk = (chunks: list(Chunk.t(t))) => {
    let n = List.length(chunks);
    assert(n > 0);
    Chain.mk(chunks, List.init(n, Fun.const()));
  };
  let empty = unit(Chunk.empty);
  let rec split_hd = (bk: t) =>
    switch (Chain.unlink(bk)) {
    | Error(Line(ln)) => Error(ln)
    | Error(Block({indent, block})) =>
      open Stds.Result.Syntax;
      let+ (hd, tl) = split_hd(block);
      (hd, Chain.unit(Block({indent, block: tl})));
    | Ok((Line(ln), (), tl)) => Ok((ln, tl))
    | Ok((Block({indent, block}), (), rest)) =>
      open Stds.Result.Syntax;
      let+ (hd, tl) = split_hd(block);
      (hd, Chain.link(Block({indent, block: tl}), (), rest));
    };
  let chunk = (~indent=0, block): Chunk.t(t) =>
    switch (Chain.unlink(block)) {
    | Error(Block({indent: n, block})) =>
      // avoid double indentation
      let indent = max(n, indent);
      Block({indent, block});
    | _ => Block({indent, block})
    };
  let chunk_body = (~indent=0, block: t): t =>
    switch (split_hd(block)) {
    | Error(_) => block
    | Ok((hd, tl)) =>
      switch (split_ft(tl)) {
      | Ok((body, ft)) when Line.is_space(ft) =>
        mk([Line(hd), chunk(~indent, body), Line(ft)])
      | _ => mk([Line(hd), chunk(~indent, tl)])
      }
    };
  let cat_ft = (block: t, line: Line.t) =>
    block
    |> Chain.map_ft(
         fun
         | Line(l) => Line(Line.cat(l, line))
         | Block({indent, block}) =>
           Block({indent, block: cat_ft(block, line)}),
       );
  let stack = (l: t, r: t) => Chain.append(l, (), r);
  let cat = (l: t, r: t) =>
    switch (split_hd(r)) {
    | Error(line) => cat_ft(l, line)
    | Ok((hd, tl)) => stack(cat_ft(l, hd), tl)
    };
  let concat = (bs: t) => List.fold_right(cat, bs, empty);
};

let of_tok = (tok: Token.t): Block.t =>
  switch (tok.mtrl) {
  | Space () =>
    Strings.split('\n', tok.text)
    |> List.map(s => Chunk.Line({space: Utf8.length(s), subst: 0}))
    |> Block.mk
  | Grout(_) => Block.unit(Chunk.Line({space: 0, subst: 1}))
  | Tile(_) => Block.unit(Chunk.Line({space: 0, subst: Token.length(tok)}))
  };

let rec of_cell = (~delim=Delim.root, c: Cell.t): Block.t =>
  switch (Cell.get(c)) {
  | None => Block.empty
  | Some(m) =>
    of_meld(m)
    |> Chain.fold_left(Fun.id, (block, tok, cell) =>
         Block.concat([block, tok, cell])
       )
    |> Block.chunk_body(~indent=Delim.indent(delim) ? 2 : 0)
  }
and of_meld = (m: Meld.t): Chain.t(Block.t, Block.t) =>
  Meld.to_chain(m)
  |> Chain.fold_left(of_cell, (dims, tok, cell) =>
       Chain.link(of_cell(~delim=Node(tok), cell), of_tok(tok), dims)
     )
  |> Chain.rev;

let rec of_cell = (~delim=Delim.root, c: Cell.t): Block.t =>
  switch (Cell.get(c)) {
  | None => Block.empty
  | Some(m) =>
    let (l, w, r) = of_meld(m);
    ();
  };
