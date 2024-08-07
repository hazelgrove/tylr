/*
     Notes about the typescript grammar

     Wherever TS defines an opt(automatic_semicolon) we are requiring a semicolon

     The pat sort is just an lhs_exp. To get a true pat you must use the top level pat and pass in a Pat.atom() as the argument. This was done due to recursion conflicts within pat/lhs_exp

     "statement_identifier" never seems to be defined in the TreeSitter grammar so we are just ignoring it and assuming it is a normal ident
 */

//TODO: Look into writing an extension to the tylr grammar to do more of a ts precedence with a table of named values and each individual form associated with a particular named value - also with the nested arrays for separate precedence levels by form/sort?

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

//let tokc_alt = ss => alt(List.map(c, ss));
let tokop_alt = ss => alt(List.map(op, ss));

let comma_sep = (r: Regex.t) => seq([r, star(seq([c(","), r]))]);

let pat = atom => seq([opt(c("...")), atom()]);

let private_property_ident = seq([c("#"), t(Id_lower)]);
let import = kw("import");

//TODO
let property_name = alt([t(Id_lower)]);

let optional_chain = c("?.");

let rest_pat = atom => seq([c("..."), atom()]);

let member_exp = exp =>
  seq([
    alt([exp(), import]),
    alt([c("."), optional_chain]),
    alt([private_property_ident, t(Id_lower)]),
  ]);

let subscript_exp = exp =>
  seq([
    alt([exp()]),
    opt(optional_chain),
    brc(L, "["),
    exp(),
    brc(R, "]"),
  ]);

//NOTE: when passing pat in to this you must reference the top level "pat" and give it a Pat.atom() as its argument

let assignment_pat =
    (
      exp: unit => Tylr_core.Regex.t(Sym.t),
      pat: unit => Tylr_core.Regex.t(Sym.t),
    ) =>
  seq([pat(), op("="), exp()]);
let array_pat = (exp, pat) =>
  seq([
    brc(L, "["),
    comma_sep(opt(alt([pat(), assignment_pat(pat, exp)]))),
    brc(R, "]"),
  ]);

let pair_pat =
    (
      exp: unit => Tylr_core.Regex.t(Sym.t),
      pat: unit => Tylr_core.Regex.t(Sym.t),
    ) =>
  seq([property_name, c(":"), alt([pat(), assignment_pat(exp, pat)])]);

let obj_assignmnet_pat = (exp, _) =>
  //NOTE: the below alt should also take in a destruct_pat but I am not dealing with that recursion. This is the best we are going to get
  seq([alt([t(Id_lower)]), c("="), exp()]);

let obj_pat =
    (
      exp: unit => Tylr_core.Regex.t(Sym.t),
      pat: unit => Tylr_core.Regex.t(Sym.t),
    ) =>
  seq([
    brc(L, "{"),
    comma_sep(
      alt([
        pair_pat(exp, pat),
        rest_pat(pat),
        obj_assignmnet_pat(exp, pat),
      ]),
    ),
    brc(R, "}"),
  ]);

let destruct_pat =
    (
      exp: unit => Tylr_core.Regex.t(Sym.t),
      pat: unit => Tylr_core.Regex.t(Sym.t),
    ) =>
  alt([obj_pat(exp, pat), array_pat(exp, pat)]);

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
and Pat: SORT = {
  let sort = () => Sort.of_str("Pat");
  let atom = () => nt(sort());

  let lhs_exp =
    alt([
      t(Id_lower),
      member_exp(Exp.atom),
      subscript_exp(Exp.atom),
      destruct_pat(Exp.atom, atom),
    ]);

  let pat = lhs_exp;

  let tbl = () => [p(pat)];
}
and Exp: SORT = {
  let sort = () => Sort.of_str("Exp");
  let atom = () => nt(sort());

  let stat_block =
    seq([brc(L, "{"), star(Stat.atom()), c(";"), brc(R, "}")]);

  let num = alt([t(Int_lit), t(Float_lit)]);

  //NOTE: for now we are making the primary same as exp atom - doing this to test our grammar designs on the assumption that treesitter separates them for precedence but our "global" exp precedence will work
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

  let member_exp = member_exp(atom);

  //TODO:assignment_pat

  let param = alt([Pat.atom() /*, assignnment_pat*/]);
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

  let obj_elements = alt([pair, spread_element /*method_def*/]);

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

  let call_exp =
    alt([
      seq([alt([atom(), import]), alt([arguments, template_string])]),
      seq([primary_exp, optional_chain, arguments]),
    ]);

  let paren_exp = seq([brc(L, "("), atom(), brc(R, ")")]);

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
      //NOTE: removed the arrow exp from the primary exp list bc they are not bi-delimited and seem
      // arrow_function,
      generator_function,
      _class,
      call_exp,
      paren_exp,
      member_exp,
    ]);

  //End of "primary" expressions
  let assignment_exp = seq([alt([paren_exp, Pat.atom()]), op("="), atom()]);
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

  //Low -> high prec
  let tbl = () =>
    [p(arrow_function)]
    @ binary_exp
    @ [
      p(ternary_exp),
      p(assignment_exp),
      p(await_exp),
      p(unary_exp),
      p(update_exp),
      p(new_exp),
      p(yield_exp),
    ]
    @ [p(operand)];
}
and Stat: SORT = {
  let sort = () => Sort.of_str("Stat");
  let atom = () => nt(sort());

  let empty_statement = c(";");
  let paren_exp = seq([brc(L, "("), Exp.atom(), brc(R, ")")]);

  let stat_block =
    seq([brc(L, "{"), star(Stat.atom()), c(";"), brc(R, "}")]);

  let param = alt([Pat.atom() /*, assignment_pat*/]);
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
  let call_signature = params;

  let module_export_name = alt([t(Id_lower) /* , t(String_lit) */]);
  let export_specifier =
    seq([module_export_name, opt(seq([kw("as"), module_export_name]))]);
  let export_clause =
    seq([
      brc(L, "{"),
      export_specifier,
      star(seq([c(","), export_specifier])),
      opt(c(",")),
      brc(R, "}"),
    ]);

  //TODO:
  // let from_clause =seq([kw("from"), t(String_lit)])
  let from_clause = seq([kw("from"), t(Id_lower)]);
  let namespace_export = seq([c("*"), kw("as"), module_export_name]);

  let export_statement =
    alt([
      seq([
        kw("export"),
        alt([
          seq([c("*"), from_clause]),
          seq([namespace_export, from_clause]),
          seq([export_clause, from_clause]),
          export_clause,
        ]),
        c(";"),
      ]),
      seq([kw("export"), atom()]),
    ]);

  let namespace_import = seq([c("*"), kw("as"), t(Id_lower)]);
  let import_specifier =
    alt([t(Id_lower), seq([module_export_name, kw("as"), t(Id_lower)])]);
  let named_imports =
    seq([
      brc(L, "{"),
      import_specifier,
      star(seq([c(","), import_specifier])),
      brc(R, "}"),
    ]);
  let import_clause =
    alt([
      namespace_import,
      named_imports,
      seq([
        t(Id_lower),
        opt(seq([c(","), alt([namespace_import, named_imports])])),
      ]),
    ]);
  let import_statement =
    seq([
      kw("import"),
      alt([seq([import_clause, from_clause]) /* , t(String) */]),
      c(";"),
    ]);

  let exp_statement = seq([Exp.atom(), c(";")]);

  let func_declaration =
    seq([
      opt(kw(~l=false, ~indent=false, "async")),
      kw(~l=false, "function"),
      opt(t(Id_lower)),
      //TODO: raise call_signature and stat block to the top level
      call_signature,
      stat_block,
      opt(c(";")),
    ]);

  let property_name = alt([t(Id_lower)]);
  let _initializer = seq([op("="), Exp.atom()]);
  let field_def =
    seq([opt(kw(~l=false, "static")), property_name, opt(_initializer)]);

  let class_static_block = seq([kw("static"), c(";"), stat_block]);
  let class_heritage = seq([kw("extends"), Exp.atom()]);
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

  let class_declaration =
    seq([
      kw(~l=false, "class"),
      opt(t(Id_lower)),
      opt(class_heritage),
      class_body,
    ]);

  let init = seq([op("="), Exp.atom()]);
  let var_declarator = seq([t(Id_lower), opt(init)]);

  let lexical_declaration =
    seq([
      alt([kw("let"), kw("const")]),
      comma_sep(var_declarator),
      c(";"),
    ]);

  let var_declaration =
    seq([kw("var"), comma_sep(var_declarator), c(";")]);

  //TODO: generator func decl
  let declaration =
    alt([
      func_declaration,
      class_declaration,
      lexical_declaration,
      var_declaration,
    ]);

  let statement_block =
    seq([brc(L, "{"), star(atom()), brc(R, "}"), opt(c(";"))]);

  let switch_case =
    seq([kw("case"), Exp.atom(), c(":"), star(Stat.atom())]);
  let switch_default = seq([kw("default"), c(":"), star(Stat.atom())]);
  let switch_body =
    seq([
      brc(L, "{"),
      star(alt([switch_case, switch_default])),
      brc(R, "}"),
    ]);
  let switch_statement = seq([kw("switch"), paren_exp, switch_body]);

  let debugger_statement = seq([kw("debugger"), c(";")]);

  let empty_statement = c(";");

  let operand =
    alt([
      debugger_statement,
      export_statement,
      import_statement,
      declaration,
      statement_block,
      switch_statement,
    ]);

  let for_statement =
    seq([
      kw("for"),
      brc(L, "("),
      alt([
        lexical_declaration,
        var_declaration,
        exp_statement,
        empty_statement,
      ]),
      alt([exp_statement, empty_statement]),
      opt(Exp.atom()),
      brc(R, ")"),
      Stat.atom(),
    ]);

  let for_header =
    seq([
      brc(L, "("),
      alt([
        alt([Pat.atom(), paren_exp]),
        seq([
          kw("var"),
          alt([t(Id_lower), destruct_pat(Exp.atom, Pat.atom)]),
          opt(init),
        ]),
        seq([
          alt([kw("let"), kw("const")]),
          alt([t(Id_lower), destruct_pat(Exp.atom, Pat.atom)]),
        ]),
      ]),
      alt([kw("in"), kw("of")]),
      Exp.atom(),
      brc(R, ")"),
    ]);

  let for_in_statement =
    seq([kw("for"), opt(kw("await")), for_header, Stat.atom()]);

  let else_clause = seq([kw("else"), Stat.atom()]);
  let if_statement =
    seq([kw("if"), paren_exp, Stat.atom(), opt(else_clause)]);

  let while_statement = seq([kw("while"), paren_exp, Stat.atom()]);

  let do_statement =
    seq([kw("do"), Stat.atom(), kw("while"), paren_exp, c(";")]);

  let catch_clause =
    seq([
      kw("catch"),
      opt(
        seq([
          brc(L, "("),
          alt([t(Id_lower), destruct_pat(Exp.atom, Pat.atom)]),
          brc(R, ")"),
        ]),
      ),
      stat_block,
    ]);
  let finally_clause = seq([kw("finally"), stat_block]);
  let try_statement =
    seq([kw("try"), stat_block, opt(catch_clause), opt(finally_clause)]);

  let with_statement = seq([kw("with"), paren_exp, Stat.atom()]);

  let break_statement = seq([kw("break"), opt(t(Id_lower)), c(";")]);

  let continue_statement =
    seq([kw("continue"), opt(t(Id_lower)), c(";")]);

  let return_statement = seq([kw("return"), opt(Exp.atom()), c(";")]);

  let throw_statement = seq([kw("throw"), Exp.atom(), c(";")]);
  
  let label_statement = seq([t(Id_lower), c(":"), Stat.atom()]);

  let tbl = () => [
    p(exp_statement),
    p(if_statement),
    p(for_statement),
    p(for_in_statement),
    p(while_statement),
    p(do_statement),
    p(try_statement),
    p(with_statement),
    p(break_statement),
    p(continue_statement),
    p(return_statement),
    p(throw_statement),
    p(operand),
  ];
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
