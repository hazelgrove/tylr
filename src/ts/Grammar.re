/*
     Notes about the typescript grammar

     Wherever TS defines an opt(automatic_semicolon) we are requiring a semicolon
 */

module Sym = {
  include Sym;
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
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

/*Indent flag is saying "do I indent next token if followed by a new line" ie
      static
          name = 1
      OR
      static
      name = 1
  */

let c = (~p=Padding.none, s) => t(Label.const(~padding=p, s));
let kw = (~l=true, ~r=true, ~indent=true) =>
  c(~p=Padding.kw(~l, ~r, ~indent, ()));
let op = (~l=true, ~r=true, ~indent=true) =>
  c(~p=Padding.op(~l, ~r, ~indent, ()));
let brc = (side: Dir.t) => c(~p=Padding.brc(side));

let tokc_alt = ss => alt(List.map(c, ss));
let tokop_alt = ss => alt(List.map(op, ss));

module type SORT = {
  let atom: unit => Regex.t;
  let sort: unit => Sort.t;
  let tbl: unit => Prec.Table.t(Regex.t);
};

module rec Typ: SORT = {
  let sort = () => Sort.of_str("Typ");
  let atom = () => nt(sort());

  let operand = alt([]);

  let tbl = () => [p(operand)];
}
and Exp: SORT = {
  let sort = () => Sort.of_str("Exp");
  let atom = () => nt(sort());

  let stat_block =
    seq([brc(L, "{"), star(Stat.atom()), c(";"), brc(R, "}")]);

  let num = alt([t(Int_lit), t(Float_lit)]);

  let lhs_exp = alt([]);
  let pat = alt([lhs_exp, kw("...")]);

  let primary_exp = atom();

  let pair = seq([t(Id_lower), c(":"), atom()]);
  let spread_element = seq([c("..."), atom()]);

  let arguments =
    seq([
      brc(L, "("),
      alt([atom(), spread_element]),
      star(seq([c(","), alt([atom(), spread_element])])),
      brc(R, ")"),
    ]);

  //TODO:assignment_pat
  let param = alt([pat]);
  let params =
    seq([brc(L, "("), param, star(seq([c(","), param])), brc(R, ")")]);
  let method_def =
    seq([
      opt(kw(~l=false, ~indent=false, "static")),
      opt(kw(~l=false, ~indent=false, "async")),
      opt(
        alt([
          kw(~l=false, ~indent=false, "get"),
          kw(~l=false, ~indent=false, "set"),
          kw(~l=false, ~indent=false, "*"),
        ]),
      ),
      t(Id_lower),
      params,
      stat_block,
    ]);

  let obj_elements = alt([pair, spread_element, method_def]);

  let obj =
    seq([
      brc(L, "{"),
      obj_elements,
      star(seq([c(","), obj_elements])),
      brc(R, "}"),
    ]);

  let arr =
    seq([
      brc(L, "["),
      opt(
        seq([
          alt([spread_element, atom()]),
          star(seq([c(","), alt([spread_element, atom()])])),
        ]),
      ),
      brc(R, "]"),
    ]);

  let call_signature = params;
  let func_exp =
    seq([
      opt(kw(~l=false, ~indent=false, "async")),
      kw(~l=false, "function"),
      opt(t(Id_lower)),
      call_signature,
      stat_block,
    ]);

  let arrow_function =
    seq([
      opt(kw(~l=false, ~indent=false, "async")),
      alt([t(Id_lower), call_signature]),
      op("=>"),
      alt([atom(), stat_block]),
    ]);

  //async function*

  let generator_function =
    seq([
      opt(kw(~l=false, ~indent=false, "async")),
      c("function"),
      kw(~l=false, "*"),
      opt(t(Id_lower)),
      call_signature,
      stat_block,
    ]);

  //TODO
  let property_name = alt([t(Id_lower)]);

  let _initializer = seq([op("="), atom()]);

  let field_def =
    seq([opt(kw(~l=false, "static")), property_name, opt(_initializer)]);

  let class_static_block = seq([kw("static"), c(";"), stat_block]);

  let class_heritage = seq([kw("extends"), atom()]);
  let class_body =
    seq([
      brc(L, "{"),
      star(
        alt([
          seq([method_def, c(";")]),
          seq([field_def, c(";")]),
          class_static_block,
          c(";"),
        ]),
      ),
      brc(R, "}"),
    ]);

  let _class =
    seq([
      kw(~l=false, "class"),
      opt(t(Id_lower)),
      opt(class_heritage),
      class_body,
    ]);

  //TODO: template strings
  let template_string = alt([]);
  let import = kw("import");
  let optional_chain = c("?.");

  //TODO: how do we want to handle atoms vs primary exp? for now just using atom
  let call_exp =
    alt([
      seq([alt([atom(), import]), alt([arguments, template_string])]),
      seq([primary_exp, optional_chain, arguments]),
    ]);

  let paren_exp = seq([brc(L, "("), atom(), brc(R, ")")]);

  let private_property_ident = seq([c("#"), t(Id_lower)]);

  let member_exp =
    seq([
      alt([atom(), primary_exp, import]),
      alt([c("."), optional_chain]),
      alt([private_property_ident, t(Id_lower)]),
    ]);

  let subscript_exp =
    seq([
      alt([atom(), primary_exp]),
      opt(optional_chain),
      brc(L, "["),
      atom(),
      brc(R, "]"),
    ]);

  let primary_exp_list =
    alt([
      t(Id_lower),
      c("this"),
      c("super"),
      num,
      //TODO: string lit
      kw("true"),
      kw("false"),
      kw("null"),
      obj,
      arr,
      func_exp,
      arrow_function,
      generator_function,
      _class,
      call_exp,
      paren_exp,
      member_exp,
    ]);

  //End of "primary" expressions
  let assignment_exp = seq([alt([paren_exp, lhs_exp]), op("="), atom()]);
  let await_exp = seq([kw("await"), atom()]);
  let unary_exp =
    seq([
      alt([
        kw("delete"),
        kw("void"),
        kw("typeof"),
        kw("+"),
        kw("-"),
        kw("~"),
        kw("!"),
      ]),
      atom(),
    ]);
  let update_exp =
    alt([
      seq([atom(), alt([c("++"), c("--")])]),
      seq([alt([c("++"), c("--")]), atom()]),
    ]);

  let logical_ops = tokop_alt(["||", "&&"]);
  let binary_shift_ops = tokop_alt([">>", ">>>", "<<"]);
  let bitwise_ops = tokop_alt(["&", "|", "^"]);
  let binary_add_ops = tokop_alt(["+", "-"]);
  let binary_times_ops = tokop_alt(["*", "/", "%"]);
  let compare_ops = tokop_alt(["<", ">", "<=", ">=", "instanceof", "in"]);
  let equality_ops = tokop_alt(["==", "!=", "===", "!=="]);

  let operand = alt([primary_exp_list]);

  let binary_exp = [
    p(seq([atom(), logical_ops, atom()])),
    p(seq([atom(), binary_shift_ops, atom()])),
    p(seq([atom(), bitwise_ops, atom()])),
    p(seq([atom(), binary_add_ops, atom()])),
    p(seq([atom(), binary_times_ops, atom()])),
    p(seq([atom(), compare_ops, atom()])),
    p(seq([atom(), equality_ops, atom()])),
    p(seq([atom(), op("??"), atom()])),
  ];

  let ternary_exp = seq([atom(), c("?"), atom(), c(":"), atom()]);

  let new_exp = seq([kw("new"), primary_exp, opt(arguments)]);
  let yield_exp =
    seq([kw("yield"), alt([seq([c("*"), atom()]), opt(atom())])]);

  let tbl = () =>
    [p(operand)]
    @ binary_exp
    @ [
      p(ternary_exp),
      p(assignment_exp),
      p(await_exp),
      p(unary_exp),
      p(update_exp),
      p(new_exp),
      p(yield_exp),
    ];
}
and Stat: SORT = {
  let sort = () => Sort.of_str("Stat");
  let atom = () => nt(sort());

  let operand = alt([]);

  let tbl = () => [p(operand)];
}
and Module: SORT = {
  let sort = () => Sort.of_str("Module");
  let atom = () => nt(sort());

  let operand = alt([]);

  let tbl = () => [p(operand)];
};

type t = Sort.Map.t(Prec.Table.t(Regex.t));
let v =
  [
    Typ.(sort(), tbl()),
    Stat.(sort(), tbl()),
    Exp.(sort(), tbl()),
    Module.(sort(), tbl()),
  ]
  |> List.to_seq
  |> Sort.Map.of_seq;
