open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Stds;

module Marks = Marks.Token;

module Base = {
  [@deriving (show({with_path: false}), sexp, yojson, hash)]
  type t('mtrl) = {
    [@hash.ignore]
    id: Id.t,
    text: string,
    marks: Marks.t,
    mtrl: 'mtrl,
  };
  let mk = (~id=?, ~text="", ~marks=?, mtrl) => {
    let id = Id.Gen.value(id);
    {id, mtrl, marks, text};
  };
  let map = (f, tok) => {...tok, mtrl: f(tok.mtrl)};
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
    | Tile((lbl, mold)) =>
      let (l, r) = Mold.display(mold);
      let text =
        switch (Label.oblig(tok.text, lbl)) {
        | exception (Invalid_argument(_)) => tok.text
        | "" => tok.text
        | s => tok.text ++ "_" ++ s ++ "_"
        };
      Fmt.pf(out, "%s%s%s", l, text, r);
    };
  let show = Fmt.to_to_string(pp);

  let mk = (~id=?, ~text="", ~marks=?, mtrl: Mtrl.T.t) =>
    Base.mk(~id?, ~text, ~marks?, mtrl);

  let empty = () => mk(Space());
  let space = () => mk(~text=" ", Space());

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

  let is_complete = (tok: t) =>
    switch (tok.mtrl) {
    | Space ()
    | Grout(_) => true
    | Tile((lbl, _)) => Label.is_complete(tok.text, lbl)
    };

  let cat = (l: t, ~caret=?, r: t) => {
    let n = Utf8.length(l.text);
    let marks = r.marks |> Marks.shift(n) |> Marks.union(l.marks);
    {...l, marks, text: l.text ++ r.text}
    |> (
      switch (caret) {
      | None => Fun.id
      | Some(hand) => add_mark(Caret.mk(hand, n))
      }
    );
  };

  let merge = (~save_cursor=?, l: t, r: t) =>
    if (l.id == r.id) {
      switch (save_cursor) {
      | None => Some(clear_marks(l))
      | Some(d) => Some(Dir.pick(d, (l, r)))
      };
    } else {
      None;
    };
  let merges = (l, r) => Option.is_some(merge(l, r));

  let is_end = (n: int, tok: t): option(Dir.t) =>
    if (n <= 0) {
      Some(L);
    } else if (n >= length(tok)
               || !is_complete(tok)
               && n > Utf8.length(tok.text)) {
      Some(R);
    } else {
      None;
    };

  let rec pop_end_carets = (tok: t): (option(Caret.t(unit)) as 'c, t, 'c) =>
    switch (tok.marks) {
    | None => (None, tok, None)
    | Some(Point(car)) =>
      switch (is_end(car.path, tok)) {
      | None => (None, tok, None)
      | Some(L) => (
          Some(Caret.map(Fun.const(), car)),
          clear_marks(tok),
          None,
        )
      | Some(R) => (
          None,
          clear_marks(tok),
          Some(Caret.map(Fun.const(), car)),
        )
      }
    | Some(Select(sel)) =>
      let (l, r) = Step.Selection.carets(sel);
      let (l_l, tok_l, r_l) = pop_end_carets(put_cursor(Point(l), tok));
      let (l_r, tok_r, r_r) = pop_end_carets(put_cursor(Point(r), tok));
      let l = Options.merge(l_l, l_r, ~f=(l, _) => l);
      let r = Options.merge(r_l, r_r, ~f=(_, r) => r);
      let tok = {...tok, marks: Marks.union(tok_l.marks, tok_r.marks)};
      (l, tok, r);
    };

  // splits token according to internal cursor.
  // carets at the ends of the token are pruned in output.
  // - (None, None, None) means there was no cursor.
  // - (Some(_), _, _) means leftmost caret strictly within token.
  // - (_, Some(_), _) means part of token is selected
  // - (_, _, Some(_)) means rightmost caret is strictly within token.
  let split = (tok: t): (option(t) as 'o, 'o, 'o) =>
    switch (tok.marks) {
    | None => (None, None, None)
    | Some(Point({path, _})) =>
      // let _ = failwith("probably get rid of mark clearing here");
      if (path <= 0) {
        (None, None, Some(clear_marks(tok)));
      } else if (path >= length(tok)) {
        (Some(clear_marks(tok)), None, None);
      } else {
        (Some(tok), None, Some(tok));
      }
    | Some(Select(sel)) =>
      let (l, r) = Step.Selection.carets(sel);
      if (l.path <= 0 && r.path >= length(tok)) {
        (None, Some(clear_marks(tok)), None);
      } else if (l.path <= 0) {
        let tok = put_cursor(Point(r), tok);
        (None, Some(tok), Some(tok));
      } else if (r.path >= length(tok)) {
        let tok = put_cursor(Point(l), tok);
        (Some(tok), Some(tok), None);
      } else {
        (Some(tok), Some(tok), Some(tok));
      };
    };

  // call this on a marked token to determine how to update the ctx
  let unzip =
      (~default=Caret.focus(), tok: t)
      : (option(t), Cursor.t(Caret.t(unit), Selection.t(t)), option(t)) => {
    let (l, popped, r) = pop_end_carets(tok);
    let cur =
      tok.marks
      |> Option.map(
           Cursor.map(Caret.map(Fun.const()), Selection.put(popped)),
         )
      |> Option.value(~default=Cursor.Point(default));
    let l =
      switch (l) {
      | Some(_) => None
      | None => Some(popped)
      };
    let r =
      switch (r) {
      | Some(_) => None
      | None => Some(popped)
      };
    (l, cur, r);
  };

  let splits = (tok: t) =>
    switch (unzip(tok)) {
    | (Some(_), _, Some(_)) => true
    | _ => false
    };

  let split_caret = (tok: t): (t, Caret.t(unit), t) =>
    switch (unzip(tok)) {
    | (Some(l), Point(hand), Some(r)) => (l, hand, r)
    | _ => raise(Invalid_argument("Token.Molded.split_caret"))
    };

  let split_text = (tok: t) =>
    tok.marks
    |> Option.map((cur: Step.Cursor.t) => {
         let (m, n) =
           switch (cur) {
           | Point({path: n, _}) => (n, n)
           | Select({range, _}) => range
           };
         Utf8.split_sub(m, n, tok.text);
       });
  let affix = (~side: Dir.t, tok: t) =>
    split_text(tok)
    |> Option.map(Dir.pick(side, (((l, _, _)) => l, ((_, _, r)) => r)))
    |> Option.value(~default=tok.text);
};
include Molded;

module Space = {
  let is = (tok: Molded.t) => Mtrl.is_space(tok.mtrl);
  let mk = (~id=?, ~text="", ~marks=?, ()) =>
    Molded.mk(~id?, ~text, ~marks?, Space());
  let empty = mk();
  // let cursor = failwith("todo Token.Space");
  let squash = (l: Molded.t, ~caret=?, r: Molded.t) => {
    ...l,
    text: l.text ++ r.text,
    marks: {
      let n = Molded.length(l);
      r.marks
      |> Marks.shift(n)
      |> Marks.union(l.marks)
      |> (
        switch (caret) {
        | None => Fun.id
        | Some(hand) => Marks.add(Caret.mk(hand, n))
        }
      );
    },
  };
};
module Grout = {
  type t = Base.t(Grout.T.t);
  let is_ = (tok: Molded.t): option(t) =>
    switch (tok.mtrl) {
    | Grout(g) => Some(Base.map(Fun.const(g), tok))
    | Space ()
    | Tile(_) => None
    };
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
  let has_lbl = (lbl: Label.t, tok: t) =>
    switch (tok.mtrl) {
    | Space ()
    | Grout () => false
    | Tile(lbls) => List.mem(lbl, lbls)
    };
  let expands = (tok: t) =>
    switch (tok.mtrl) {
    | Space ()
    | Grout () => None
    | Tile(lbls) =>
      let expanding_lbls =
        lbls
        |> List.filter(
             fun
             // todo: add expands flag to label
             | Label.Const(_, text) when text == tok.text => true
             | _ => false,
           );
      switch (expanding_lbls) {
      | [] => None
      | [_, ..._] => Some({...tok, mtrl: Mtrl.Tile(expanding_lbls)})
      };
    };
};

let unmold = (tok: Molded.t): Unmolded.t => {
  let mtrl =
    switch (tok.mtrl) {
    | Space () =>
      switch (Labels.completions(tok.text)) {
      | [] => Mtrl.Space()
      | [_, ..._] as lbls => Tile(lbls)
      }
    | Grout(_) => raise(Invalid_argument("Token.Unmolded.unmold"))
    | Tile((lbl, _)) =>
      Tile(is_empty(tok) ? [lbl] : Labels.completions(tok.text))
    };
  Unmolded.mk(~id=tok.id, ~text=tok.text, mtrl);
};
