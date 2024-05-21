open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Util;

module Marks = {
  [@deriving (show({with_path: false}), sexp, yojson, hash)]
  type t = list((int, bool));
  let shift = n => List.map(((m, b)) => (m + n, b));
  let union = (@);
};

module Base = {
  [@deriving (show({with_path: false}), sexp, yojson, hash)]
  type t('mtrl) = {
    [@hash.ignore]
    id: Id.t,
    mtrl: 'mtrl,
    marks: Marks.t,
    text: string,
  };
  let mk = (~id=?, ~text="", ~marks=[], mtrl) => {
    let id = Id.Gen.value(id);
    {id, mtrl, marks, text};
  };
  let id = (tok: t(_)) => tok.id;
  let is_empty = (tok: t(_)) => String.equal(tok.text, "");
  let add_mark = (mark, tok) => {...tok, marks: [mark, ...tok.marks]};
};

module Molded = {
  include Base;
  [@deriving (sexp, yojson)]
  type t = Base.t(Mtrl.T.t);

  let pp = (out, tok: t) =>
    switch (tok.mtrl) {
    | Space () => Fmt.pf(out, "|%s|", tok.text)
    | Grout((_, tips)) =>
      let (l, r) = Tip.display(tips);
      Fmt.pf(out, "%s%s", l, r);
    | Tile((_, mold)) =>
      let (l, r) = Mold.display(mold);
      Fmt.pf(out, "%s%s%s", l, tok.text, r);
    };
  let show = Fmt.to_to_string(pp);

  let mk = (~id=?, ~text="", ~marks=[], mtrl: Mtrl.T.t) =>
    Base.mk(~id?, ~text, ~marks, mtrl);

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
  let unzip = (n: int, tok: t): Result.t((t, t), Dir.t) =>
    switch (tok.mtrl, Utf8.split(n, tok.text)) {
    | (_, ("", _)) => Error(L)
    | (Space () | Grout(_), (_, "")) => Error(R)
    | (Space () | Grout(_), (l, r)) =>
      Ok(({...tok, text: l}, {...tok, text: r}))
    | (Tile((lbl, _)), (_, ""))
        when Label.is_complete(tok.text, lbl) || n > Utf8.length(tok.text) =>
      Error(R)
    | (Tile(_), (txt_l, txt_r)) =>
      let l = {...tok, text: txt_l};
      let r = {...tok, text: txt_r};
      Ok((l, r));
    };
  let pull = (~from: Dir.t, tok: t): option((t, t)) => {
    let n = Dir.pick(from, (1, length(tok) - 1));
    Result.to_option(unzip(n, tok));
  };
};
include Molded;

module Space = {
  let is = (tok: Molded.t) => Mtrl.is_space(tok.mtrl);
  let mk = (~id=?, ~text="", ~marks=[], ()) =>
    Molded.mk(~id?, ~text, ~marks, Space());
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
  let is_unfinished = (tok: t) =>
    switch (tok.mtrl) {
    | Space ()
    | Grout(_) => false
    | Tile((lbl, _)) => !Label.is_complete(tok.text, lbl)
    };
};

module Unmolded = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Base.t(Mtrl.t(unit, unit, list(Label.t)));
  let mk = (~id=?, ~text="", mtrl: Mtrl.t(_)): t =>
    Base.mk(~id?, ~text, mtrl);
  let unmold = (tok: Molded.t): t => {
    let mtrl =
      switch (tok.mtrl) {
      | Space () => Mtrl.Space()
      | Grout(_) => raise(Invalid_argument("Token.Unmolded.unmold"))
      | Tile((lbl, _)) =>
        Tile(
          is_empty(tok)
            ? [lbl] : Labels.completions(Label.const(tok.text)),
        )
      };
    mk(~id=tok.id, ~text=tok.text, mtrl);
  };
  let defer = (tok: t): Molded.t => Space.mk(~id=tok.id, ~text=tok.text, ());
};
