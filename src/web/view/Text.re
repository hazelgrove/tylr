open Virtual_dom.Vdom;
open Tylr_core;

let nest = (n: int) =>
  n == 0
    ? Fun.id
    : List.cons(
        Node.div(
          ~attrs=Attr.[class_("indent")],
          [Node.text(Stds.Strings.repeat(n, Util.Unicode.nbsp))],
        ),
      );

let view_space = spc =>
  String.to_seq(spc)
  |> Seq.map(
       fun
       | ' ' => Util.Unicode.nbsp
       | c => String.init(1, Fun.const(c)),
     )
  |> List.of_seq
  |> String.concat("");

let view_tok = (tok: Token.t) => {
  let mtrl_clss =
    switch (tok.mtrl) {
    // todo: distinguish whitespace from unmolded styling
    | Space(White(_)) => ["space"]
    | Space(Unmolded) => ["unmolded"]
    | Grout(_) => ["grout"]
    | Tile((_, mold)) =>
      Mold.(t_nullable(~side=L, mold) && t_nullable(~side=R, mold))
        ? ["tile"] : ["tile", "match"]
    };
  let attrs =
    Attr.[
      classes(["token", ...mtrl_clss]),
      title(Sexplib.Sexp.to_string_hum(Token.sexp_of_t(tok))),
    ];
  let nodes =
    switch (tok.mtrl) {
    // todo: distinguish whitespace from unmolded styling
    | Space(_) => [Node.text(view_space(tok.text))]
    | Grout(_) => [Node.text("•")]
    | Tile((lbl, _)) =>
      let text = Node.text(tok.text);
      let oblig = Label.oblig(tok.text, lbl);
      if (Base.String.is_empty(oblig)) {
        [Node.text(tok.text)];
      } else {
        let ghost =
          Node.span(~attrs=Attr.[class_("ghost")], [Node.text(oblig)]);
        [text, ghost];
      };
    };
  Node.span(~attrs, nodes);
};

let view_line = (l: Block.Line.t) =>
  l |> List.map(view_tok) |> Node.span(~attrs=[Attr.class_("line")]);

let rec view_block = (B(b): Block.t) =>
  b
  |> Chain.fold_left_map(
       sec => ((), view_sec(sec)),
       ((), indent, sec) => ((), [Node.br()], view_sec(~indent, sec)),
     )
  |> snd
  |> Chain.to_list(Fun.id, Fun.id)
  |> List.concat
  |> Node.div(~attrs=Attr.[classes(["block"])])
and view_sec = (~indent=0, sec: Block.Section.t(_)) =>
  nest(
    indent,
    [
      switch (sec) {
      | Line(l) => view_line(l)
      | Block(b) => view_block(b)
      },
    ],
  );

// module Chunk = {
//   type t('block) =
//     | Line(list(Node.t))
//     | Indented(int, block);
// };
// module Block = {
//   include Chain;
//   type t = Chain.t(Chunk.t(t), unit);
//   let chunks = Chain.loops;
// };

// let view_str =
//   Stds.Memo.general(
//     fun
//     | "" => []
//     | str => Node.[span([text(str)])],
//   );
// let view_indent =
//   Stds.Memo.general(n => view_str(Strings.repeat(n, Util.Unicode.nbsp)));

// let rec view = (block: Block.t) =>
//   Block.chunks(block)
//   |> List.map(
//        fun
//        | Chunk.Line(nodes) =>
//          Node.div(~attrs=Attr.[classes(["chunk", "line"])], nodes)
//        | Indented(indent, block) =>
//          Node.div(
//            ~attrs=Attr.[classes(["chunk", "indented"])],
//            view_indent(indent) @ [view(block)],
//          ),
//      )
//   |> Node.div(~attrs=Attr.[classes(["block"])]);
