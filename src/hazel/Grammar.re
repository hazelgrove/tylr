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
  let typ = nt(sort)


  let comma_sep = seq([typ, Star(seq([c(","), typ]))]);

  let operand = 
    alt([
      c("Int"),
      c("Float"),
      c("Bool"),
      c("String"),
      //List type
      seq([c("list"), c("("), typ, c(")")]),
      //Tuple type
      seq([c("("), comma_sep, c(")")]),
    ])

  let tbl = [
      //Arrow
      p(seq([typ, c("->"), typ])),
      //Ap
      p(seq([typ, c("("), typ, c(")")])),
      p(operand)
  ];
};

module Pat = {
  let sort = Sort.of_str("Pat");
  let pat = nt(sort)

  let comma_sep = seq([pat, Star(seq([c(","), pat]))]);


  let bool_lit = alt([c("true"), c("false")])
  let operand = 
    alt([
      t(Int_lit),
      t(Float_lit),
      t(Id_lower),
      bool_lit,
      //Constructor
      t(Id_upper),
      seq([c("("), comma_sep, c(")")]),
      seq([c("["), comma_sep, c("]")]),
      seq([c("\""), t(Id_lower), c("\"")]),
      //Wild
      c("_"),
      //Empty hole
      seq([c("{"), c("}")]),
      //Non-empty hole
      seq([c("{"), pat, c("}")])
    ])


  let tbl = [
    //Typeann
    //Was one of the limitations not ending with a non-terminal of a different sort? bc if so, this breaks that rule
    p(seq([pat, c(":"), nt(Typ.sort)])), 
    //ap
    p(seq([pat, c("("), pat, c(")")])),
    //Cons
    p(seq([pat, c("::"), pat])),
    p(operand)          
  ];
};

module Exp = {
  let sort = Sort.of_str("Exp");
  let exp = nt(sort);

  [@warning "-32"]
  let comma_sep = seq([exp, Star(seq([c(","), exp]))]);

  let rul = seq([c("|"), nt(Pat.sort), c("=>"), exp])
  let bool_lit = alt([c("true"), c("false")])

  let operand =
    alt([
      t(Int_lit),
      t(Float_lit),
      t(Id_lower),
      bool_lit,
      // todo: seq([tokc("("), opt(comma_sep), tokc(")")]),
      seq([c("("), comma_sep, c(")")]),
      // todo: seq([tokc("["), opt(comma_sep), tokc("]")]),
      seq([c("["), comma_sep, c("]")]),
      // seq([tokc("case"), e, Star(rule), tokc("end")]),
      //TODO: figure out how to do the string better? 
      seq([c("\""), t(Id_lower), c("\"")]),
            //fun
      seq([c("fun"), nt(Pat.sort), c("->"), exp]),
      //if
      seq([c("if"), exp, c("then"), exp, c("else"), exp]),
      //case
      seq([c("case"), exp, rul, star(rul)]),
    ]);

  let tokc_alt = ss => alt(List.map(c, ss));
  let add_op = tokc_alt(["+", "+.", "-", "-."]);
  let mult_op = tokc_alt(["*", "*.", "/", "/."]);
  let neg_op = tokc_alt(["-", "-."]);

  //TODO: ask david if this is a valid way to do this?
  let concat_op = tokc_alt(["@", "++"])



  let tbl = [
    p(~a=L, seq([exp, add_op, exp])),
    p(~a=L, seq([exp, mult_op, exp])),
    //List/string concat ops
    p(~a=L, seq([exp, concat_op, exp])),
    p(seq([neg_op, exp])),
    //ap
    p(seq([exp, c("("), exp, c(")")])),
    p(operand),
  ];
};

type t = Sort.Map.t(Prec.Table.t(Regex.t));
let v =
  [Typ.(sort, tbl), Pat.(sort, tbl), Exp.(sort, tbl)]
  |> List.to_seq
  |> Sort.Map.of_seq;
