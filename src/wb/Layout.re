open Sexplib.Std;
open Util;
open Cor;

type tip_shape = (Tip.shape, int);
type selem_shape = (tip_shape, tip_shape);

type t =
  | Text(string)
  | Cat(t, t)
  | Annot(annot, t)
and annot =
  | Delim
  | EmptyHole(option(Sort.t))
  | Space(option(caret))
  | ClosedChild
  | OpenChild
  | UniChild(Sort.t, Direction.t)
  | Selem(option(Sort.t), selem_shape, SelemStyle.t)
// | Selected
and caret =
  | Pointing
  | Selecting
  | Restructuring(t);

let empty = Text("");

let annot = (annot, l) => Annot(annot, l);

let cat = (l1, l2) => Cat(l1, l2);
let cats =
  fun
  | [] => empty
  | [l, ...ls] => List.fold_left(cat, l, ls);

let join = (sep: t, ls: list(t)) => ls |> ListUtil.join(sep) |> cats;

let delim = s => annot(Delim, Text(s));
let empty_hole = (~sort=?, ()) =>
  annot(EmptyHole(sort), Text(Unicode.nbsp));
let open_child = annot(OpenChild);
let closed_child = annot(ClosedChild);
let uni_child = (~sort, ~side) => annot(UniChild(sort, side));

let space = (~caret=?, ()) => Annot(Space(caret), Text(Unicode.nbsp));
let spaces = join(space());

let spaces_padded = ls => cats([space(), spaces(ls), space()]);
let spaces_padded_l = ls => cat(space(), spaces(ls));
let spaces_padded_r = ls => cat(spaces(ls), space());
let spaces_padded_z = (ls_pre, caret, ls_suf) =>
  cats([
    spaces_padded_l(ls_pre),
    space(~caret, ()),
    spaces_padded_r(ls_suf),
  ]);

let length = {
  let rec go =
    lazy(
      Memo.memoize(
        fun
        | Text(s) => Unicode.length(s)
        | Cat(l1, l2) => Lazy.force(go, l1) + Lazy.force(go, l2)
        | Annot(_, l) => Lazy.force(go, l),
      )
    );
  Lazy.force(go);
};

type measurement = {
  start: int,
  len: int,
};

let measured_fold' =
    (
      ~text: (measurement, string) => 'acc,
      ~cat: (measurement, 'acc, 'acc) => 'acc,
      // let client cut off recursion
      ~annot: (t => 'acc, measurement, annot, t) => 'acc,
      ~start=0,
      l: t,
    ) => {
  let rec go = (~start, l: t) => {
    let m = {start, len: length(l)};
    switch (l) {
    | Text(s) => text(m, s)
    | Cat(l1, l2) =>
      let mid = start + length(l1);
      cat(m, go(~start, l1), go(~start=mid, l2));
    | Annot(ann, l) => annot(go(~start), m, ann, l)
    };
  };
  go(~start, l);
};

let measured_fold = (~annot: (measurement, annot, 'acc) => 'acc, ~start=0) =>
  measured_fold'(~annot=(k, m, ann, l) => annot(m, ann, k(l)), ~start);

let rec place_caret = (d: Direction.t, caret, l) =>
  switch (l) {
  | Text(_) => l
  | Cat(l1, l2) =>
    switch (d) {
    | Left => Cat(place_caret(d, caret, l1), l2)
    | Right => Cat(l1, place_caret(d, caret, l2))
    }
  | Annot(Space(_), l) => Annot(Space(Some(caret)), l)
  | Annot(annot, l) => Annot(annot, place_caret(d, caret, l))
  };

type with_dangling_caret = (t, option(Direction.t));

let place_caret_0: option((caret, CaretPosition.t)) => option(Direction.t) =
  Option.map(
    fun
    | (_, CaretPosition.Before(_)) => Direction.Left
    | (_, After) => Right,
  );
let place_caret_1 = (caret, child1) =>
  switch (caret) {
  | None => (child1, None)
  | Some((_, CaretPosition.Before(0))) => (child1, Some(Direction.Left))
  | Some((caret, Before(_one))) => (
      place_caret(Right, caret, child1),
      None,
    )
  | Some((_, After)) => (child1, Some(Right))
  };
let place_caret_2 = (caret, child1, child2) =>
  switch (caret) {
  | None => (child1, child2, None)
  | Some((_, CaretPosition.Before(0))) => (
      child1,
      child2,
      Some(Direction.Left),
    )
  | Some((caret, Before(1))) => (
      place_caret(Right, caret, child1),
      child2,
      None,
    )
  | Some((caret, Before(_two))) => (
      child1,
      place_caret(Right, caret, child2),
      None,
    )
  | Some((_, After)) => (child1, child2, Some(Right))
  };

let mk_Paren = (~caret: option((caret, CaretPosition.t))=?, body) => {
  let (body, dangling_caret) = place_caret_1(caret, body);
  let l = cats([delim("("), open_child(body), delim(")")]);
  (l, dangling_caret);
};

let mk_Lam = (~caret=?, p) => {
  let (p, dangling_caret) = place_caret_1(caret, p);
  let l = cats([delim(Unicode.lam), closed_child(p), delim(".")]);
  (l, dangling_caret);
};

let mk_Let = (~caret=?, p, def) => {
  let (p, def, dangling_caret) = place_caret_2(caret, p, def);
  let l =
    cats([
      delim("let"),
      closed_child(p),
      delim("="),
      open_child(def),
      delim("in"),
    ]);
  (l, dangling_caret);
};

let mk_Plus = (~caret=?, ()) => (delim("+"), place_caret_0(caret));
let mk_Times = (~caret=?, ()) => (delim("*"), place_caret_0(caret));

let mk_Prod = (~caret=?, ()) => (delim(","), place_caret_0(caret));

let mk_OpHole = (~caret=?, ~sort=?, ()) => (
  empty_hole(~sort?, ()),
  place_caret_0(caret),
);

let mk_BinHole = (~caret=?, ~sort=?, ()) => (
  empty_hole(~sort?, ()),
  place_caret_0(caret),
);

let mk_text = (~caret=?, s) => (Text(s), place_caret_0(caret));

let mk_token =
  Token.get(
    fun
    | Token_pat.Paren_l => delim("(")
    | Paren_r => delim(")"),
    fun
    | Token_exp.Paren_l => delim("(")
    | Paren_r => delim(")")
    | Lam_lam => delim(Unicode.lam)
    | Lam_dot => delim(".")
    | Let_let => delim("let")
    | Let_eq => delim("=")
    | Let_in => delim("in"),
  );

let rec mk_tiles = ts => spaces(List.map(t => fst(mk_tile(t)), ts))
and mk_tile = (~caret=?, t) =>
  t
  |> Tile.get(
       fun
       | Tile_pat.OpHole => mk_OpHole(~caret?, ~sort=Pat, ())
       | Var(x) => mk_text(~caret?, x)
       | Paren(body) =>
         // TODO undo unnecessary rewrapping
         mk_Paren(~caret?, mk_tiles(Tiles.of_pat(body)))
       | BinHole => mk_BinHole(~caret?, ~sort=Pat, ())
       | Prod => mk_Prod(~caret?, ()),
       fun
       | Tile_exp.OpHole => mk_OpHole(~caret?, ~sort=Exp, ())
       | Num(n) => mk_text(~caret?, string_of_int(n))
       | Var(x) => mk_text(~caret?, x)
       | Paren(body) => mk_Paren(~caret?, mk_tiles(Tiles.of_exp(body)))
       | Lam(p) => mk_Lam(~caret?, mk_tiles(Tiles.of_pat(p)))
       | Let(p, def) =>
         mk_Let(
           ~caret?,
           mk_tiles(Tiles.of_pat(p)),
           mk_tiles(Tiles.of_exp(def)),
         )
       | BinHole => mk_BinHole(~caret?, ~sort=Exp, ())
       | Plus => mk_Plus(~caret?, ())
       | Times => mk_Times(~caret?, ())
       | Prod => mk_Prod(~caret?, ()),
     );

let selem_shape = selem => {
  let (lshape, _) = Selem.tip(Left, selem);
  let (rshape, _) = Selem.tip(Right, selem);
  let ltails = Selem.tails(Left, selem);
  let rtails = Selem.tails(Right, selem);
  ((lshape, ltails), (rshape, rtails));
};

let mk_selem = (~sort=?, ~style: option(SelemStyle.t)=?, selem: Selem.t) => {
  let l = selem |> Selem.get(mk_token, tile => fst(mk_tile(tile)));
  switch (style) {
  | None => l
  | Some(style) => annot(Selem(sort, selem_shape(selem), style), l)
  };
};

let mk_selection = (~sort=?, ~style: option(SelemStyle.t)=?, selection) =>
  spaces(List.map(mk_selem(~sort?, ~style?), selection));

let zip_up =
    (subject: Selection.t, frame: Frame.t)
    : option((int, Tile.t, ListFrame.t(Tile.t), Frame.t)) => {
  let get_pat = selection =>
    selection
    |> Selection.get_tiles
    |> Option.get
    |> Tiles.get_pat
    |> Option.get;
  let get_exp = selection =>
    selection
    |> Selection.get_tiles
    |> Option.get
    |> Tiles.get_exp
    |> Option.get;
  switch (frame) {
  | Pat(Paren_body((tframe, frame))) =>
    let body = get_pat(subject);
    let tframe = TupleUtil.map2(Tiles.of_pat, tframe);
    Some((1, Pat(Paren(body)), tframe, Pat(frame)));
  | Pat(Lam_pat((tframe, frame))) =>
    let p = get_pat(subject);
    let tframe = TupleUtil.map2(Tiles.of_exp, tframe);
    Some((1, Exp(Lam(p)), tframe, Exp(frame)));
  | Pat(Let_pat(def, (tframe, frame))) =>
    let p = get_pat(subject);
    let tframe = TupleUtil.map2(Tiles.of_exp, tframe);
    Some((1, Exp(Let(p, def)), tframe, Exp(frame)));
  | Exp(Paren_body((tframe, frame))) =>
    let body = get_exp(subject);
    let tframe = TupleUtil.map2(Tiles.of_exp, tframe);
    Some((1, Exp(Paren(body)), tframe, Exp(frame)));
  | Exp(Let_def(p, (tframe, frame))) =>
    let def = get_exp(subject);
    let tframe = TupleUtil.map2(Tiles.of_exp, tframe);
    Some((2, Exp(Let(p, def)), tframe, Exp(frame)));
  | Exp(Root) => None
  };
};

let get_uni_children =
    (root_tile: Tile.t, (prefix, _) as tframe: ListFrame.t(Tile.t))
    : (ListFrame.t(Tile.t), ListFrame.t(Tile.t)) => {
  let tiles = ListFrame.to_list(~subject=[root_tile], tframe);
  let skel = Parser.associate(tiles);
  let m = List.length(prefix);
  let subskel = Skel.skel_at(List.length(prefix), skel);
  let (n, _) as range = Skel.range(subskel);
  let (subtiles, tframe) = ListFrame.split_sublist(range, tiles);
  let (_, uni_children) = ListFrame.split_nth(m - n, subtiles);
  (uni_children, tframe);
};

let mk_frame = (_frame: Frame.t): (t => t) => failwith("todo");

let mk_pointing = (sframe: Selection.frame, frame: Frame.t): t => {
  let sort = Frame.sort(frame);
  let uni_child = uni_child(~sort);
  let mk_subject =
      (
        ~caret: CaretPosition.t,
        root_tile: Tile.t,
        (child_pre, child_suf): ListFrame.t(Tile.t),
        (prefix, suffix): ListFrame.t(Tile.t),
      ) => {
    let (root_tile, dangling_caret) =
      mk_tile(~caret=(Pointing, caret), root_tile)
      |> PairUtil.map_fst(
           annot(Selem(Some(sort), selem_shape(Tile(root_tile)), Root)),
         );
    let child_pre = uni_child(~side=Left, mk_tiles(List.rev(child_pre)));
    let child_suf = uni_child(~side=Right, mk_tiles(child_suf));
    let prefix = mk_tiles(List.rev(prefix));
    let suffix = mk_tiles(suffix);
    switch (dangling_caret) {
    | None => spaces_padded([prefix, child_pre, root_tile, child_suf, suffix])
    | Some(Left) =>
      spaces_padded_z(
        [prefix, child_pre],
        Pointing,
        [root_tile, child_suf, suffix],
      )
    | Some(Right) =>
      spaces_padded_z(
        [prefix, child_pre, root_tile],
        Pointing,
        [child_suf, suffix],
      )
    };
  };
  let tframe =
    sframe
    |> TupleUtil.map2(Selection.get_tiles)
    |> TupleUtil.map2(Option.get);
  switch (tframe) {
  | (prefix, []) =>
    switch (zip_up(Selection.of_tiles(List.rev(prefix)), frame)) {
    | None =>
      let (root_tile, prefix) = ListUtil.split_first(prefix);
      let (uni_children, tframe) =
        get_uni_children(root_tile, (prefix, []));
      mk_frame(
        frame,
        mk_subject(~caret=After, root_tile, uni_children, tframe),
      );
    | Some((delim_index, root_tile, tframe, frame)) =>
      let (uni_children, tframe) = get_uni_children(root_tile, tframe);
      mk_frame(
        frame,
        mk_subject(
          ~caret=Before(delim_index),
          root_tile,
          uni_children,
          tframe,
        ),
      );
    }
  | (prefix, [root_tile, ...suffix]) =>
    let (uni_children, tframe) =
      get_uni_children(root_tile, (prefix, suffix));
    mk_frame(
      frame,
      mk_subject(~caret=Before(0), root_tile, uni_children, tframe),
    );
  };
};
