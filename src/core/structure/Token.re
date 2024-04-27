open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Util;

module Base = {
  [@deriving (show({with_path: false}), sexp, yojson, hash)]
  type t('mtrl, 'mold) = {
    [@hash.ignore]
    id: Id.t,
    mtrl: 'mtrl,
    mold: 'mold,
    text: string,
  };
  let mk = (~id=?, ~text="", mtrl, mold) => {
    let id = Id.Gen.value(id);
    {id, mtrl, mold, text};
  };
  let id = (tok: t(_)) => tok.id;
  let is_empty = (tok: t(_)) => String.equal(tok.text, "");
};

module Molded = {
  include Base;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Base.t(Mtrl.T.t, Mold.t);

  let mk = (~id=?, ~text="", mtrl: Mtrl.T.t, mold: Mold.t) =>
    Base.mk(~id?, ~text, mtrl, mold);

  let indent = (tok: t) => Mtrl.Labeled.padding(tok.mtrl).indent;
  let sort = (tok: t) => tok.mold.sort;
  let length = (tok: t) =>
    switch (tok.mtrl) {
    | Tile(Const(_, c)) => String.length(c)
    | _ => String.length(tok.text)
    };

  let merge = (l: t, r: t) => {...l, text: l.text ++ r.text};
  let zip = (l: t, r: t) =>
    if (Id.eq(l.id, r.id)) {
      assert(Mold.equal(l.mold, r.mold));
      Some({...l, text: l.text ++ r.text});
    } else {
      None;
    };
  let unzip = (n: int, tok: t): Result.t((t, t), Dir.t) =>
    switch (tok.mtrl, StringUtil.unzip_opt(n, tok.text)) {
    | (_, Some(("", _))) => Error(L)
    | (Space | Grout, Some((_, ""))) => Error(R)
    | (Space | Grout, Some((l, r))) =>
      Ok(({...tok, text: l}, {...tok, text: r}))
    | (Space | Grout, None) => raise(Invalid_argument("Token.unzip"))
    | (Tile(lbl), Some((_, ""))) when Label.is_complete(tok.text, lbl) =>
      Error(R)
    | (Tile(_), Some((txt_l, txt_r))) =>
      let l = {...tok, text: txt_l};
      let r = {...tok, text: txt_r};
      Ok((l, r));
    | (Tile(lbl), None) =>
      n > 0 && !Label.is_complete(tok.text, lbl)
        ? Error(R) : raise(Invalid_argument("Token.unzip"))
    };
  let pull = (~from: Dir.t, tok: t): option((t, t)) => {
    let n = Dir.pick(from, (1, length(tok) - 1));
    Result.to_option(unzip(n, tok));
  };
};
include Molded;

let is_ghost = (tok: t) =>
  switch (tok.mtrl) {
  | Space
  | Grout => false
  | Tile(lbl) => !Label.is_complete(tok.text, lbl)
  };

module Space = {
  let is = (tok: Molded.t) => Mtrl.is_space(tok.mtrl);
  let mk = (~id=?, ~text="", ()) =>
    Molded.mk(~id?, ~text, Space, Space.Mold.of_t);
  let empty = mk();
  // let cursor = failwith("todo Token.Space");
};
module Grout = {
  let is = (tok: Molded.t) => Mtrl.is_grout(tok.mtrl);
  let text = " ";
  let mk = (~id=?, mold: Mtrl.Sorted.t => Mold.t, s: Mtrl.Sorted.t) =>
    Molded.mk(~id?, ~text, Mtrl.Grout, mold(s));
  let op_ = (~id=?) => mk(~id?, Grout.Mold.T.op_);
  let pre = (~id=?) => mk(~id?, Grout.Mold.T.pre);
  let pos = (~id=?) => mk(~id?, Grout.Mold.T.pos);
  let in_ = (~id=?) => mk(~id?, Grout.Mold.T.in_);
};

module Unmolded = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Base.t(Mtrl.t(list(Label.t)), unit);
  let mk = (~id=?, ~text="", mtrl: Mtrl.t(list(Label.t))) =>
    Base.mk(~id?, ~text, mtrl, ());
  let unmold = (tok: Molded.t): t => {
    let mtrl =
      switch (tok.mtrl) {
      | Space => Mtrl.Space
      | Grout => Grout
      | Tile(lbl) =>
        Tile(
          is_empty(tok)
            ? [lbl] : Labels.completions(Label.const(tok.text)),
        )
      };
    mk(~id=tok.id, ~text=tok.text, mtrl);
  };
  let defer = (tok: t): Molded.t => Space.mk(~id=tok.id, ~text=tok.text, ());
};
