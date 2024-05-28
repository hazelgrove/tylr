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



module Pat = {
  let sort = Sort.of_str("Pat");
  let atom = nt(sort);
  let tbl = [];
};

module type SORT = {
    let atom: Regex.t;
    let sort: Sort.t;
    let tbl: Prec.Table.t(Regex.t);
}

module rec Stat: SORT = {
  let sort = Sort.of_str("Stat");
  let atom = nt(sort);

  let tbl = [p(seq([Exp.atom, c(";")]))];
}
and Typ: SORT = {
  let sort = Sort.of_str("Typ");
  let atom = nt(sort);
  let tbl = [];
}
and Item: SORT = {
  let sort = Sort.of_str("Item");
  let atom = nt(sort);

  [@warning "-32"]
  let comma_sep = seq([atom, Star(seq([c(","), atom]))]);


  let func_qualifier = alt([c("const"), c("async"), c("unsafe")])
  
  let self_param = alt([
    //Shorthand self
    seq([c("&"), opt(c("mut")), c("self")]),
    //Typed self
    seq([opt(c("mut")), c("self"), c(":"), Typ.atom])
  ])
  let func_param = alt([seq([t(Id_lower), c(":"), alt([Typ.atom, c("...")])]), c("...")])
  let func_params = alt([self_param, seq([
    opt(seq([self_param, c(",")])),
    func_param, star(seq([c(","), func_param])),
    opt(c(","))
  ])])

  let func_return_typ = seq([c("->"), Typ.atom])

  //TODO: ask David how to handle sucessive c(...); should I use padding?
  let func = seq([opt(func_qualifier), c("fn"), t(Id_lower), c("("), opt(func_params), c(")"), func_return_typ, alt([
    //TODO: replace this with block exp
    Exp.atom,
    c(";")
  ])]);

  let operand = alt([
      func,
  ])

  let tbl = [
      p(operand)
  ];
}
and Exp: SORT = {
  let sort = Sort.of_str("Exp");
  let atom = nt(sort);

  [@warning "-32"]
  let comma_sep = seq([atom, Star(seq([c(","), atom]))]);

  let block_exp = seq([c("{"), Stat.atom, c("}")]);

  let lone_if_exp = seq([c("if"), atom, block_exp]);
  let if_exp =
    seq([
      lone_if_exp,
      opt(
        seq([c("else"), star(seq([lone_if_exp, c("else")])), block_exp]),
      ),
      //NOTE: initial idea for non-recurisve if decl; limited by ambiguity with the "else"s in the molds
      // star(seq([c("else"), exp, block_exp])),
      // opt(seq([c("else"), block_exp])),
    ]);


  //TODO: ask David how the handle the "scrutinee" type (all exps except struct exp)
  let loop_exp = [
      //Infinite loop
      seq([c("loop"), block_exp]),
      //Predicate (while) loops
      seq([c("while"), atom, block_exp]),
      //TODO: Predicate pattern (while let) loops - do we have support for successive terminals?
      // seq([])

      //iterator (for) loops
      seq([c("for"), Pat.atom, c("in"), atom, block_exp]),
  ]

  let exp_with_block = [if_exp, block_exp];

  let operand =
    alt(
      [
        t(Int_lit),
        t(Float_lit),
        t(Id_lower),
        //TODO: ask David how these lone keywords should be handeled
        c("break"),
        c("continue"),
        //Function call
        seq([atom, c("("), opt(comma_sep), c(")")]),
        //Parenthetical expression
        seq([c("("), atom, c(")")]),
        //Arrays
        seq([c("["), atom, opt(star(atom)), c("]")]),
      ]
      @ exp_with_block,
    );

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

  //NOTE: tbl goes from weak -> strong precedence
  //NOTE: exp without block > exp with block (prec)
  let tbl = [
    //return
    p(seq([c("return"), atom])),
    //assignment ops
    p(seq([atom, assignment_ops, atom])),
    //bool or
    p(seq([atom, c("||"), atom])),
    //bool and
    p(seq([atom, c("&&"), atom])),
    //comparison ops
    p(seq([atom, compare_ops, atom])),
    //add
    p(~a=L, seq([atom, add_ops, atom])),
    //mult
    p(~a=L, seq([atom, mult_ops, atom])),
    //type cast exp
    p(~a=L, seq([atom, c("as"), Typ.atom])),
    //unary operators
    p(seq([unary_op, atom])),
    //question mark op
    p(seq([c("?"), atom])),
    p(operand),
  ];

};

type t = Sort.Map.t(Prec.Table.t(Regex.t));
let v =
  [Typ.(sort, tbl), Pat.(sort, tbl), Exp.(sort, tbl)]
  |> List.to_seq
  |> Sort.Map.of_seq;
