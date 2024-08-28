/* Notes about deviations from the Rust grammar

      * Rust defines if expressions recursively - we unfortunately cannot and had to do a combination of alts and stars
      * Rust disallows struct expressions from the scrutinee type (all exp except struct expression) - we assume this is a parser limitation and allow generic exp
      * Block exp - rust references block exp in other "sorts" (items) - we instead "rebuild" the block exp definition (bracket, stat, bracket) for each sort where it is used
      * Rust comparison operators "require parentheses for associativity" - using tylr assoc=none for this
      * Rust has optional semicolons after block exps for a statement - we are always requiring semicolons to simplify so we don't need to discriminate b/w exp with block and exp without block
      * Rust allows for optional commas following exp_with_block in a match statement - for consistency sake we will reqire comma in match regardless of exp_with_block or exp_without_block
      * Rust has a separate "type no bounds" which only allows for no bound or single bounded types. This is used only in reference types and raw pointer types. Our current guess is that this is a type system limitation so we are (for now) ignoring it and simply allowing any type where rust only allows "type no bounds"
      * Rust defines statement as an alt one option of which is just "Item" - to allow for this in our grammar definition we are making item a submodule of statement and inlining all the forms of item into statement's operand. However, the item _module form requires "Item.atom" so we will keep Item as a tylr sort.
   */

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

let plus = (reg: Regex.t) => seq([reg, star(reg)]);

let p = (~a: option(Dir.t)=?, r: t) => (a, r);

let t = (lbl: Label.t) => Regex.atom(Sym.t(lbl));
let nt = (srt: Sort.t) => Regex.atom(Sym.nt(srt));

let c = (~p=Padding.none, s) => t(Label.const(~padding=p, s));

//TODO: abi should actually be a string literal but need to wait on David for that
let abi = t(Id_lower);

//Path exps
let path_ident_segment =
  alt([
    t(Id_lower),
    c("super"),
    c("self"),
    c("Self"),
    c("crate"),
    c("$crate"),
  ]);

module Filter = {
  //Whitelisted strings
  type t = list(string);
};

module type SORT = {
  let atom: (~filter: Filter.t=?, unit) => Regex.t;

  let sort: unit => Sort.t;
  let tbl: unit => Prec.Table.t(Regex.t);
};

module rec Stat: SORT = {
  let sort = () => Sort.of_str("Stat");
  let atom = (~filter as _: Filter.t=[], ()) => nt(sort());

  let block_exp = seq([c("{"), plus(atom()), c("}")]);

  let let_stat =
    seq([
      c("let"),
      Pat.atom(),
      opt(seq([c(":"), Typ.atom()])),
      opt(seq([c("="), Exp.atom(), opt(seq([c("else"), block_exp]))])),
      c(";"),
    ]);

  let operand = alt([let_stat, Item.atom()]);
  let tbl = () => [p(seq([Exp.atom(), c(";")])), p(operand)];
}
and Item: SORT = {
  let sort = () => Sort.of_str("Item");
  let atom = (~filter as _: Filter.t=[], ()) => nt(sort());

  [@warning "-32"]
  let comma_sep = seq([atom(), star(seq([c(","), atom()]))]);
  let block_exp = seq([c("{"), plus(atom()), c("}")]);

  //Functions!
  let func_qualifier = alt([c("const"), c("async"), c("unsafe")]);

  let self_param =
    alt([
      //Shorthand self
      seq([c("&"), opt(c("mut")), c("self")]),
      //Typed self
      seq([opt(c("mut")), c("self"), c(":"), Typ.atom()]),
    ]);
  let func_param =
    alt([
      seq([t(Id_lower), c(":"), alt([Typ.atom(), c("...")])]),
      c("..."),
    ]);
  let func_params =
    alt([
      self_param,
      seq([
        opt(seq([self_param, c(",")])),
        func_param,
        star(seq([c(","), func_param])),
        opt(c(",")),
      ]),
    ]);
  let func_return_typ = seq([c("->"), Typ.atom()]);

  let func =
    seq([
      opt(func_qualifier),
      c("fn"),
      t(Id_lower),
      c("("),
      opt(func_params),
      c(")"),
      func_return_typ,
      alt([block_exp, c(";")]),
    ]);
  //End functions

  //Crates
  let extern_crate =
    seq([
      c("extern"),
      c("crate"),
      alt([t(Id_lower), c("self")]),
      opt(seq([c("as"), alt([t(Id_lower), c("_")])])),
      c(";"),
    ]);

  //Module
  let _module =
    alt([
      seq([opt(c("unsafe")), c("mod"), t(Id_lower), c("l")]),
      seq([
        opt(c("unsafe")),
        c("mod"),
        t(Id_lower),
        c("{"),
        star(atom()),
        c("}"),
      ]),
    ]);

  let typ_alias =
    seq([
      c("type"),
      t(Id_lower),
      opt(seq([c("="), Typ.atom()])),
      c(";"),
    ]);

  //Struct
  let struct_field = seq([t(Id_lower), c(":"), Typ.atom()]);
  let struct_fields =
    seq([struct_field, star(seq([c(","), struct_field])), opt(c(","))]);
  let struct_struct =
    seq([
      c("struct"),
      t(Id_lower),
      alt([seq([c("{"), opt(struct_fields), c("}")]), c(";")]),
    ]);

  let tuple_field = Typ.atom();
  let tuple_fields =
    seq([tuple_field, star(seq([c(","), tuple_field])), opt(c(","))]);
  let tuple_struct =
    seq([
      c("struct"),
      t(Id_lower),
      c("("),
      opt(tuple_fields),
      c(")"),
      c(";"),
    ]);
  let _struct = alt([struct_struct, tuple_struct]);

  //Enums
  let enum_item_tuple = seq([c("("), opt(tuple_fields), c(")")]);
  let enum_item_struct = seq([c("{"), opt(struct_fields), c("}")]);
  let enum_item =
    seq([
      t(Id_lower),
      opt(alt([enum_item_tuple, enum_item_struct])),
      opt(seq([c("="), Exp.atom()])),
    ]);
  let enum_items =
    seq([enum_item, star(seq([c(","), enum_item])), opt(c(","))]);
  let enum =
    seq([c("enum"), t(Id_lower), c("{"), opt(enum_items), c("}")]);

  //Unions
  let union =
    seq([c("union"), t(Id_lower), c("{"), struct_fields, c("}")]);

  //Consts
  let const =
    seq([
      c("const"),
      alt([t(Id_lower), c("_")]),
      c(":"),
      Typ.atom(),
      opt(seq([c("="), Exp.atom()])),
      c(";"),
    ]);

  //static
  let static_item =
    seq([
      c("static"),
      opt(c("mut")),
      t(Id_lower),
      c(":"),
      opt(seq([c("="), Exp.atom()])),
      c(";"),
    ]);

  //Traits
  let associated_item = alt([typ_alias, const, func]);
  let trait =
    seq([
      opt(c("unsafe")),
      c("trait"),
      t(Id_lower),
      c("{"),
      star(associated_item),
      c("}"),
    ]);

  //Implementations
  let inherent_impl =
    seq([c("impl"), c("{"), star(associated_item), c("}")]);

  let trait_impl =
    seq([
      opt(c("unsafe")),
      c("impl"),
      opt(c("!")),
      Typ.atom(~filter=["typ_path"], ()),
      c("for"),
      Typ.atom(),
    ]);

  let impl = alt([inherent_impl, trait_impl]);

  //Externs
  let extern_item = alt([static_item, func]);
  let extern_block =
    seq([
      opt(c("unsafe")),
      c("extern"),
      opt(abi),
      c("{"),
      star(extern_item),
      c("}"),
    ]);

  let operand =
    alt([
      extern_block,
      impl,
      trait,
      static_item,
      const,
      union,
      enum,
      _struct,
      typ_alias,
      func,
      extern_crate,
      _module,
    ]);

  let tbl = () => [p(operand)];
}
and Typ: SORT = {
  let sort = () => Sort.of_str("Typ");
  let atom = (~filter as _: Filter.t=[], ()) => nt(sort());

  let typ_path_fn_inputs =
    seq([atom(), star(seq([c(","), atom()])), opt(c(","))]);
  let typ_path_fn =
    seq([
      c("("),
      opt(typ_path_fn_inputs),
      c(")"),
      opt(seq([c("->"), atom()])),
    ]);
  let typ_path_segment =
    seq([path_ident_segment, opt(seq([c("::"), typ_path_fn]))]);
  let typ_path =
    seq([
      opt(c("::")),
      typ_path_segment,
      star(seq([c("::"), typ_path_segment])),
    ]);

  let paren_typ = seq([c("("), atom(), c(")")]);

  let trait_bound = seq([opt(c("?")), typ_path]);
  //TODO: lifetimes
  let typ_param_bound = trait_bound; //Lifetime | TraitBound

  let typ_param_bounds =
    seq([
      typ_param_bound,
      star(seq([c("+"), typ_param_bound])),
      opt(c("+")),
    ]);

  let impl_trait_typ = seq([c("impl"), typ_param_bounds]);
  let impl_trait_typ_one_bound = seq([c("impl"), trait_bound]);

  let trait_obj_typ_one_bound = seq([opt(c("dyn")), trait_bound]);
  let trait_obj_typ = seq([opt(c("dyn")), typ_param_bounds]);

  let tuple_typ =
    alt([
      seq([c("("), c(")")]),
      seq([c("("), plus(seq([atom(), c(",")])), opt(atom()), c(")")]),
    ]);

  let never_typ = c("!");

  let raw_pointer_typ =
    seq([c("*"), alt([c("mut"), c("const")]), atom()]);

  //TODO: lifetimes
  let reference_typ = seq([c("&"), opt(c("mut")), atom()]);

  let array_typ = seq([c("["), atom(), c(";"), Exp.atom(), c("]")]);
  let slice_typ = seq([c("["), atom(), c("]")]);

  let inferred_typ = c("_");

  let qualified_path_typ =
    seq([c("<"), atom(), opt(seq([c("as"), typ_path])), c(">")]);
  let qualified_path_in_typ =
    seq([qualified_path_typ, plus(seq([c("::"), typ_path_segment]))]);

  let fun_typ_qualifiers =
    seq([opt(c("unsafe")), opt(seq([c("extern"), opt(abi)]))]);

  let maybe_named_param =
    seq([opt(seq([alt([t(Id_lower), c("_")]), c(":")])), atom()]);
  let maybe_named_fun_params_variadic =
    seq([
      star(seq([maybe_named_param, c(",")])),
      maybe_named_param,
      c(","),
      c("..."),
    ]);
  let maybe_named_fun_params =
    seq([
      maybe_named_param,
      star(seq([c(","), maybe_named_param])),
      opt(c(",")),
    ]);
  let fun_params_maybe_named_variadic =
    alt([maybe_named_fun_params, maybe_named_fun_params_variadic]);

  let bare_fun_return_typ = seq([c("->"), atom()]);

  let bare_fun_typ =
    seq([
      fun_typ_qualifiers,
      c("fn"),
      c("("),
      opt(fun_params_maybe_named_variadic),
      opt(bare_fun_return_typ),
    ]);

  let operand =
    alt([
      impl_trait_typ,
      trait_obj_typ,
      paren_typ,
      impl_trait_typ_one_bound,
      trait_obj_typ_one_bound,
      typ_path,
      tuple_typ,
      never_typ,
      raw_pointer_typ,
      reference_typ,
      array_typ,
      slice_typ,
      inferred_typ,
      qualified_path_in_typ,
      bare_fun_typ,
    ]);

  let tbl = () => [p(operand)];
}
and Exp: SORT = {
  let sort = () => Sort.of_str("Exp");
  let atom = (~filter as _: Filter.t=[], ()) => nt(sort());

  [@warning "-32"]
  let comma_sep = seq([atom(), star(seq([c(","), atom()]))]);

  let block_exp = seq([c("{"), Stat.atom(), c("}")]);

  let lone_if_exp = seq([c("if"), atom(), block_exp]);
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

  let loop_exp = [
    //Infinite loop
    seq([c("loop"), block_exp]),
    //Predicate (while) loops
    seq([c("while"), atom(), block_exp]),
    //Predicate pattern (while let) loops
    seq([c("while"), c("let"), Pat.atom(), c("="), Exp.atom(), block_exp]),
    //iterator (for) loops
    seq([c("for"), Pat.atom(), c("in"), atom(), block_exp]),
  ];

  let match_arm = seq([Pat.atom(), opt(seq([c("if"), atom()]))]);

  let match_arms =
    seq([
      star(seq([match_arm, c("=>"), atom(), c(",")])),
      match_arm,
      c("=>"),
      atom(),
      opt(c(",")),
    ]);

  let match_exp =
    seq([c("match"), atom(), c("{"), opt(match_arms), c("}")]);

  let exp_with_block = [match_exp] @ loop_exp @ [if_exp, block_exp];

  let operand =
    alt(
      [
        t(Int_lit),
        t(Float_lit),
        t(Id_lower),
        c("break"),
        c("continue"),
        //Function call
        seq([atom(), c("("), opt(comma_sep), c(")")]),
        //Parenthetical expression
        seq([c("("), atom(), c(")")]),
        //Arrays
        seq([c("["), atom(), opt(star(atom())), c("]")]),
      ]
      @ exp_with_block,
    );

  let tokc_alt = ss => alt(List.map(c, ss));
  let unary_op = tokc_alt(["&", "&mut", "*", "-", "!"]);
  let mult_ops = tokc_alt(["*", "/", "%"]);
  let add_ops = tokc_alt(["+", "-"]);

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
  let tbl = () => [
    //return
    p(seq([c("return"), atom()])),
    //assignment ops
    p(seq([atom(), assignment_ops, atom()])),
    //bool or
    p(seq([atom(), c("||"), atom()])),
    //bool and
    p(seq([atom(), c("&&"), atom()])),
    //comparison ops
    p(seq([atom(), compare_ops, atom()])),
    //add
    p(~a=L, seq([atom(), add_ops, atom()])),
    //mult
    p(~a=L, seq([atom(), mult_ops, atom()])),
    //type cast exp
    p(~a=L, seq([atom(), c("as"), Typ.atom()])),
    //unary operators
    p(seq([unary_op, atom()])),
    //question mark op
    p(seq([c("?"), atom()])),
    p(operand),
  ];
}
and Pat: SORT = {
  let sort = () => Sort.of_str("Pat");
  let atom = (~filter as _: Filter.t=[], ()) => nt(sort());

  let ident_pat =
    seq([
      opt(c("ref")),
      opt(c("mut")),
      t(Id_lower),
      opt(seq([c("@"), atom()])),
    ]);

  //NOTE: actual rust grammar has a star(outer_attr) then c(..) but since we don't have metaprogramming or attrs, struct_pat_et_cetera is just .. for us
  let struct_pat_et_cetera = c("..");

  let struct_pat_field =
    alt([
      seq([t(Int_lit), c(":"), atom()]),
      seq([t(Id_lower), c("l"), atom()]),
      seq([opt(c("ref")), opt(c("mut")), t(Id_lower)]),
    ]);
  let struct_pat_fields =
    seq([struct_pat_field, star(seq([c(","), struct_pat_field]))]);
  let struct_pat_elements =
    alt([
      seq([
        struct_pat_fields,
        opt(alt([c(","), seq([c(","), struct_pat_et_cetera])])),
      ]),
      struct_pat_et_cetera,
    ]);

  let path_exp_segment = seq([path_ident_segment]);
  let path_in_exp =
    seq([
      opt(c("::")),
      path_exp_segment,
      star(seq([c("::"), path_exp_segment])),
    ]);

  let struct_pat =
    seq([path_in_exp, c("{"), opt(struct_pat_elements), c("}")]);

  let tuple_struct_items =
    seq([atom(), star(seq([c(","), atom()])), opt(c(","))]);
  let tuple_struct_pat =
    seq([path_in_exp, c("("), opt(tuple_struct_items), c(")")]);

  let rest_pat = c("..");
  let tuple_pat_items =
    alt([
      seq([atom(), c(",")]),
      rest_pat,
      seq([atom(), star(seq([c(","), atom(), opt(c(","))]))]),
    ]);
  let tuple_pat = seq([c("("), tuple_pat_items, c(")")]);

  let group_pat = seq([c("("), atom(), c(")")]);

  let slice_pat_items =
    seq([atom(), star(seq([c(","), atom()])), opt(c(","))]);
  let slice_pat = seq([c("["), opt(slice_pat_items), c("]")]);

  let qualified_path_in_exp =
    seq([
      Typ.atom(~filter=["qualified_path_typ"], ()),
      plus(seq([c("::"), path_exp_segment])),
    ]);

  let path_exp = alt([path_in_exp, qualified_path_in_exp]);
  let path_pat = path_exp;

  let operand =
    alt([
      c("true"),
      c("false"),
      //TODO: char literal, string literal
      seq([opt(c("-")), t(Int_lit)]),
      seq([opt(c("-")), t(Float_lit)]),
      //Wild
      c("_"),
      //Rest
      rest_pat,
      //Reference
      seq([alt([c("&"), c("&&")]), opt(c("mut")), atom()]),
      ident_pat,
      struct_pat,
      tuple_struct_pat,
      tuple_pat,
      group_pat,
      slice_pat,
      path_pat,
    ]);

  let tbl = () => [p(operand)];
};

type t = Sort.Map.t(Prec.Table.t(Regex.t));
let v =
  //TODO: ask david is this a valid way to do the item?
  [
    Typ.(sort(), tbl()),
    Pat.(sort(), tbl()),
    Exp.(sort(), tbl()),
    Stat.(sort(), tbl()),
    Item.(sort(), tbl()),
  ]
  |> List.to_seq
  |> Sort.Map.of_seq;
