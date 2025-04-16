module Sym = {
  include Sym;
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Sym.t(Label.t, (Filter.t, Sort.t));
};
module Regex = {
  include Regex;
  type t = Regex.t(Sym.t);
};
open Regex;

let p = (~a: option(Dir.t)=?, r: t) => (a, r);

let t = (lbl: Label.t) => Regex.atom(Sym.t(lbl));
let nt = (filter: Filter.t, srt: Sort.t) =>
  Regex.atom(Sym.nt((filter, srt)));

let c = (~p=Padding.none, ~i=false, s) =>
  t(Label.const(~padding=p, ~instant=i, s));
let kw = (~space=(true, true), ~break=(false, false), ~indent=true) =>
  c(~p=Padding.kw(~space, ~break, ~indent, ()));
let op = (~space=(true, true), ~break=(false, false), ~indent=false) =>
  c(~p=Padding.op(~space, ~break, ~indent, ()));
let brc = (side: Dir.t) => c(~p=Padding.brc(side), ~i=true);

let comma = op(~space=(false, true), ~i=true, ",");
let comma_sep = atom => seq([atom, Star(seq([comma, atom]))]);

module type SORT = {
  let atom: (~filter: Filter.t=?, unit) => Regex.t;
  let sort: unit => Sort.t;
  let tbl: unit => Prec.Table.t(Regex.t);
};

module rec Typ: SORT = {
  let sort = () => Sort.of_str("Typ");
  let atom = (~filter=[], ()) => nt(filter, sort());
  // let typ = nt(sort);

  let cons_ap = () => seq([atom(), brc(L, "("), atom(), brc(R, ")")]);

  let operand = () =>
    alt([
      // c("Int"),
      // c("Float"),
      // c("Bool"),
      // c("String"),
      t(Id_upper),
      //List type
      seq([brc(L, "["), atom(), brc(R, "]")]),
      //seq([c("list"), brc(L, "("), atom(), brc(R, ")")]),
      //Tuple type
      seq([brc(L, "("), atom(), brc(R, ")")]),
    ]);

  let tbl = () => [
    // Product
    p(comma_sep(atom())),
    // Arrow
    p(~a=R, seq([atom(), op("->"), atom()])),
    // Sum
    p(~a=L, seq([atom(), op("+"), atom()])),
    //Constructor def for sums
    p(cons_ap()),
    p(operand()),
  ];
}
and Pat: SORT = {
  let sort = () => Sort.of_str("Pat");
  let atom = (~filter=[], ()) => nt(filter, sort());

  // let bool_lit = alt([c("true"), c("false")]);

  let cons_ap = () => seq([atom(), brc(L, "("), atom(), brc(R, ")")]);
  let operand = () =>
    alt([
      t(Int_lit),
      t(Float_lit),
      t(Id_lower),
      // bool_lit,
      //Constructor
      t(Id_upper),
      seq([brc(L, "("), atom(), brc(R, ")")]),
      seq([brc(L, "["), atom(), brc(R, "]")]),
      //Wild
      c("_"),
    ]);

  let tbl = () => [
    p(comma_sep(atom())),
    //Typeann
    p(seq([atom(), kw(~space=(false, true), ":"), Typ.atom()])),
    //Cons
    p(~a=R, seq([atom(), op(~space=(false, false), "::"), atom()])),
    //bare tuple
    //p(~a=L, seq([atom(), c(","), atom()])),
    //ap
    p(cons_ap()),
    p(operand()),
  ];
}
and Exp: SORT = {
  let sort = () => Sort.of_str("Exp");
  let atom = (~filter=[], ()) => nt(filter, sort());

  // let bool_lit = alt([c("true"), c("false")]);

  let rul = () =>
    seq([kw(~break=(true, false), "|"), Pat.atom(), kw("=>"), atom()]);
  let case = () =>
    seq([
      kw(~space=(false, true), "case"),
      atom(),
      rul(),
      star(rul()),
      kw(~space=(true, false), ~break=(true, false), "end"),
    ]);

  let let_ = () =>
    seq([
      kw("let", ~space=(false, true)),
      Pat.atom(),
      kw("="),
      atom(),
      kw("in", ~break=(false, true), ~indent=false),
      atom(),
    ]);

  let type_def = () =>
    seq([
      kw("type", ~space=(false, true)),
      Typ.atom(),
      kw("="),
      Typ.atom(),
      kw("in", ~break=(false, true), ~indent=false),
      atom(),
    ]);

  let operand = () =>
    alt([
      t(Int_lit),
      t(Float_lit),
      t(Id_lower),
      t(Id_upper), // constructors
      // bool_lit,
      seq([brc(L, "("), atom(), brc(R, ")")]),
      seq([brc(L, "["), atom(), brc(R, "]")]),
    ]);

  let op_alt = ((), ~space=(true, true), ss) =>
    alt(List.map(op(~space), ss));
  let add_op = () => op_alt((), ["+", "+.", "-", "-.", "@", "++"]);
  let mult_op = () => op_alt((), ["*", "*.", "/", "/."]);
  let neg_op = () => op_alt((), ~space=(false, false), ["-", "-."]);
  let comp_op = () =>
    op_alt(
      (),
      ["<", "<=", ">", ">=", "==", "!="]
      @ ["<.", "<=.", ">.", ">=.", "==.", "!=."],
    );

  let fn_ap = () => seq([atom(), brc(L, "("), atom(), brc(R, ")")]);

  let tbl = () => [
    p(comma_sep(atom())),
    //case
    p(case()),
    //let
    p(let_()),
    p(type_def()),
    //fun
    p(
      seq([kw(~space=(false, true), "fun"), Pat.atom(), kw("=>"), atom()]),
    ),
    //if
    p(
      seq([
        kw(~space=(false, true), "if"),
        atom(),
        kw("then"),
        atom(),
        kw("else"),
        atom(),
      ]),
    ),
    p(~a=R, seq([atom(), op("||"), atom()])),
    p(~a=R, seq([atom(), op("&&"), atom()])),
    //Comparison
    p(~a=L, seq([atom(), comp_op(), atom()])),
    //Reverse-ap
    p(~a=L, seq([atom(), op("|>"), atom()])),
    //Math operations
    p(~a=L, seq([atom(), add_op(), atom()])),
    p(~a=L, seq([atom(), mult_op(), atom()])),
    p(seq([neg_op(), atom()])),
    //ap
    p(fn_ap()),
    p(operand()),
  ];
};

type t = Sort.Map.t(Prec.Table.t(Regex.t));
let v =
  [Typ.(sort(), tbl()), Pat.(sort(), tbl()), Exp.(sort(), tbl())]
  |> List.to_seq
  |> Sort.Map.of_seq;
