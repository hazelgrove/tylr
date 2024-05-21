open Util;

module Sym = {
  include Sym;
  type t = Sym.t(Label.t, Sort.t);
};
module Regex = {
  include Regex;
  type t = Regex.t(Sym.t);
};
open Regex;

let p = (~a: option(Dir.t)=?, r: t) => (a, r);

let t = (lbl: Label.t) => Regex.atom(Sym.t(lbl));
let nt = (srt: Sort.t) => Regex.atom(Sym.nt(srt));

let c = (~p=Padding.none, s) => t(Label.const(~padding=p, s));

module Typ = {
  let sort = Sort.of_str("Typ");
  let typ = nt(sort);
  let tbl = [];
};

module Pat = {
  let sort = Sort.of_str("Pat");
  let tbl = [];
};

module Stat = {
  let sort = Sort.of_str("Stat");
  let stat = nt(sort);

  let tbl = [];
};

module Item = {
  let sort = Sort.of_str("Item");
  let item = nt(sort);

  let tbl = [
    //function declaration will be our item
    p(seq([])),
  ];
};

module Exp = {
  let sort = Sort.of_str("Exp");
  let exp = nt(sort);

  [@warning "-32"]
  let comma_sep = seq([exp, Star(seq([c(","), exp]))]);

  let operand =
    alt([
      t(Int_lit),
      t(Float_lit),
      t(Id_lower),
      //Parenthetical expression
      seq([c("("), exp, c(")")]),
      //Arrays
      seq([c("["), exp, opt(star(exp)), c("]")])
    ]);

  let tokc_alt = ss => alt(List.map(c, ss));
  let unary_op = tokc_alt(["&", "&mut", "*", "-", "!"]);
  let mult_ops = tokc_alt(["*", "/", "%"]);
  let add_ops = tokc_alt(["+", "-"]);

  //TODO: ask David; rust says these ops "require parens" for the prec; does this mean that ~a=None?
  let compare_ops = tokc_alt(["==", "!=", ">", "<", ">=", "<="]);

  let assignment_ops =
    tokc_alt([
      "=",
      "+=",
      "-=",
      "*=",
      "/=",
      "%=",
      "&=",
      "|=",
      "^=",
      "<<=",
      ">>=",
    ]);
  let jump_ops = tokc_alt(["return", "break"]);

  let exp_without_block = [
    //cfg jumps
    p(seq([jump_ops, exp])),
    //assignment ops
    p(seq([exp, assignment_ops, exp])),
    //bool or
    p(seq([exp, c("||"), exp])),
    //bool and
    p(seq([exp, c("&&"), exp])),
    //comparison ops
    p(seq([exp, compare_ops, exp])),
    //add
    p(~a=L, seq([exp, add_ops, exp])),
    //mult
    p(~a=L, seq([exp, mult_ops, exp])),
    //type cast exp
    p(~a=L, seq([exp, c("as"), Typ.typ])),
    //unary operators
    p(seq([unary_op, exp])),
    //question mark op
    p(seq([c("?"), exp])),
    p(operand),
  ]


  let block_exp = seq([c("{"), Stat.stat, c("}")])

  let lone_if_exp = seq([c("if"), exp, block_exp])
  let if_exp = seq([
     lone_if_exp, opt(
        alt([
            seq([c("else"), block_exp]),
            seq([c("else"), lone_if_exp]),
            ])
        )
  ])

  let exp_with_block = [
    p(block_exp),
  ]

  //NOTE: tbl goes from weak -> strong precedence
  //NOTE: exp without block > exp with block (prec)
  let tbl = exp_with_block @ exp_without_block
};

type t = Sort.Map.t(Prec.Table.t(Regex.t));
let v =
  [Typ.(sort, tbl), Pat.(sort, tbl), Exp.(sort, tbl)]
  |> List.to_seq
  |> Sort.Map.of_seq;
