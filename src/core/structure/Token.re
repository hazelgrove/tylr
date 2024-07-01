open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Stds;

module Marks = Marks.Token;

module Base = {
  [@deriving (show({with_path: false}), sexp, yojson, hash)]
  type t('mtrl) = {
    [@hash.ignore]
    id: Id.t,
    mtrl: 'mtrl,
    marks: Marks.t,
    text: string,
  };
  let mk = (~id=?, ~text="", ~marks=?, mtrl) => {
    let id = Id.Gen.value(id);
    {id, mtrl, marks, text};
  };
  let id = (tok: t(_)) => tok.id;
  let is_empty = (tok: t(_)) => String.equal(tok.text, "");
  let add_mark = (p, tok) => {...tok, marks: Marks.add(p, tok.marks)};
  // let add_mark = (mark, tok) => {...tok, marks: [mark, ...tok.marks]};
  // let add_marks = (marks, tok) => {...tok, marks: marks @ tok.marks};
  let put_cursor = (cursor: Step.Cursor.t, tok) => {
    ...tok,
    marks: Some(cursor),
  };
  let put_marks = (marks, tok) => {...tok, marks};
  let clear_marks = tok => put_marks(None, tok);
  let pop_marks = tok => (tok.marks, clear_marks(tok));
};

module Molded = {
  include Base;
  [@deriving (sexp, yojson)]
  type t = Base.t(Mtrl.T.t);

  let pp = (out, tok: t) =>
    switch (tok.mtrl) {
    | Space () =>
      String.to_seq(tok.text)
      |> Seq.map(
           fun
           | ' ' => "_"
           | '\n' => "\\n"
           | c => String.init(1, Fun.const(c)),
         )
      |> List.of_seq
      |> String.concat("")
      |> Fmt.pf(out, "\"%s\"")
    | Grout((_, tips)) =>
      let (l, r) = Tip.display(tips);
      Fmt.pf(out, "%s%s", l, r);
    | Tile((_lbl, mold)) =>
      let (l, r) = Mold.display(mold);
      // Fmt.pf(out, "%s%a%s", l, Label.pp, lbl, r);
      Fmt.pf(out, "%s%s%s", l, tok.text, r);
    };
  let show = Fmt.to_to_string(pp);

  let mk = (~id=?, ~text="", ~marks=?, mtrl: Mtrl.T.t) =>
    Base.mk(~id?, ~text, ~marks?, mtrl);

  let is_empty = (tok: t) =>
    switch (tok.mtrl) {
    | Grout(_) => false
    | _ => is_empty(tok)
    };

  let indent = (tok: t) => Mtrl.T.padding(tok.mtrl).indent;
  let sort = tok =>
    Mtrl.map(
      ~space=Fun.id,
      ~grout=fst,
      ~tile=((_, m: Mold.t)) => m.sort,
      tok.mtrl,
    );
  let length = (tok: t) =>
    switch (tok.mtrl) {
    | Grout(_) => 1
    | Tile((Const(_, c), _)) => Utf8.length(c)
    | Tile(_)
    | Space () => Utf8.length(tok.text)
    };

  let merge = (l: t, r: t) => {
    let marks = Marks.(union(l.marks, shift(Utf8.length(l.text), r.marks)));
    {...l, marks, text: l.text ++ r.text};
  };
  let zip = (l: t, r: t) =>
    if (Id.eq(l.id, r.id)) {
      assert(Mold.equal(l.mtrl, r.mtrl));
      Some(merge(l, r));
    } else {
      None;
    };

  let is_complete = (tok: t) =>
    switch (tok.mtrl) {
    | Space ()
    | Grout(_) => true
    | Tile((lbl, _)) => Label.is_complete(tok.text, lbl)
    };

  // beware calling this on partial tokens
  let unzip = (tok: t) =>
    tok.marks
    |> Option.map((cur: Step.Cursor.t) => {
         let (m, n) =
           switch (cur) {
           | Point({path: n, _}) => (n, n)
           | Select({range, _}) => range
           };
         let (l, m, r) = Utf8.split_sub(m, n, tok.text);
         let l = Strings.is_empty(l) ? None : Some({...tok, text: l});
         let cur =
           switch (cur) {
           | Point({hand, _}) => Cursor.Point(Caret.mk(hand, ()))
           | Select(sel) =>
             Strings.is_empty(m)
               ? Point(Caret.focus())
               : Select(Selection.put({...tok, text: m}, sel))
           };
         let r =
           Strings.is_empty(r)
           && (is_complete(tok) || n > Utf8.length(tok.text))
             ? None : Some({...tok, text: r});
         (l, cur, r);
       });

  let split_caret = (tok: t): (t, Caret.t(unit), t) =>
    switch (unzip(tok)) {
    | Some((Some(l), Point(hand), Some(r))) => (l, hand, r)
    | _ => raise(Invalid_argument("Token.Molded.split_caret"))
    };

  let pull = (~from: Dir.t, tok: t): option((t, t)) =>
    if (is_empty(tok) || length(tok) == 1) {
      None;
    } else {
      let car = Caret.focus(Dir.pick(from, (1, length(tok) - 1)));
      let (l, _, r) = split_caret({...tok, marks: Some(Point(car))});
      Some((l, r));
    };
};
include Molded;

module Space = {
  let is = (tok: Molded.t) => Mtrl.is_space(tok.mtrl);
  let mk = (~id=?, ~text="", ~marks=?, ()) =>
    Molded.mk(~id?, ~text, ~marks?, Space());
  let empty = mk();
  // let cursor = failwith("todo Token.Space");
};
module Grout = {
  let is = (tok: Molded.t) => Mtrl.is_grout(tok.mtrl);
  let mk = (~id=?, tips: Tip.s, s) =>
    Molded.mk(~id?, Mtrl.Grout((s, tips)));
  let op_ = (~id=?) => mk(~id?, (Conv, Conv));
  let pre = (~id=?) => mk(~id?, (Conv, Conc));
  let pos = (~id=?) => mk(~id?, (Conc, Conv));
  let in_ = (~id=?) => mk(~id?, (Conc, Conc));
};
module Tile = {
  let is_ghost = (tok: t) =>
    switch (tok.mtrl) {
    | Space ()
    | Grout(_) => None
    | Tile((lbl, _) as t) =>
      Label.is_complete(tok.text, lbl) ? None : Some(t)
    };
};

module Unmolded = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Base.t(Mtrl.t(unit, unit, list(Label.t)));
  let mk = (~id=?, ~text="", mtrl: Mtrl.t(_)): t =>
    Base.mk(~id?, ~text, mtrl);
  let defer = (tok: t): Molded.t => Space.mk(~id=tok.id, ~text=tok.text, ());
};

let unmold = (tok: Molded.t): Unmolded.t => {
  let mtrl =
    switch (tok.mtrl) {
    | Space () => Mtrl.Space()
    | Grout(_) => raise(Invalid_argument("Token.Unmolded.unmold"))
    | Tile((lbl, _)) =>
      Tile(is_empty(tok) ? [lbl] : Labels.completions(tok.text))
    };
  Unmolded.mk(~id=tok.id, ~text=tok.text, mtrl);
};
