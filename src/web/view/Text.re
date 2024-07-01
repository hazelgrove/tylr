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

let view_tile = (lbl: Label.t, text: string) => {
  let oblig = Label.oblig(text, lbl);
  let ghost =
    Base.String.is_empty(oblig)
      ? []
      : [Node.span(~attrs=Attr.[class_("ghost")], [Node.text(oblig)])];
  Node.span(~attrs=Attr.[class_("tile")], [Node.text(text), ...ghost]);
};

let view_tok = (tok: Token.t) =>
  (
    switch (tok.mtrl) {
    | Space () =>
      Node.span(
        ~attrs=Attr.[class_("space")],
        [Node.text(view_space(tok.text))],
      )
    | Grout(_) =>
      Node.span(~attrs=Attr.[class_("grout")], [Node.text("•")])
    | Tile((lbl, _)) => view_tile(lbl, tok.text)
    }
  )
  |> Util.Nodes.add_class("token");

let view_line = (l: Layout.Block.Line.t) =>
  l |> List.map(view_tok) |> Node.span(~attrs=[Attr.class_("line")]);

let rec view_block = (B(b): Layout.Block.t) =>
  b
  |> Chain.fold_left_map(
       sec => ((), view_sec(sec)),
       ((), indent, sec) => ((), Node.br(), view_sec(~indent, sec)),
     )
  |> snd
  |> Chain.to_list(Fun.id, Fun.id)
  |> Node.div(~attrs=Attr.[classes(["block"])])
and view_sec = (~indent=0, sec: Layout.Block.Section.t(_)) => {
  let text =
    switch (sec) {
    | Line(l) => view_line(l)
    | Block(b) => view_block(b)
    };
  Node.div(~attrs=Attr.[classes(["section"])], nest(indent, [text]));
};

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
