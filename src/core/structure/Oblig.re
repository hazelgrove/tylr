open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Stds;

let debug = ref(false);

module Ord = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Missing_meld // convex grout
    | Missing_tile // ghost tile
    | Incon_meld // pre/postfix grout
    | Extra_meld // infix grout
    | Incomplete_tile
    | Unmolded_tok
    | Reserved_keyword;

  // low to high severity
  let all = [
    Missing_meld,
    Missing_tile,
    Incomplete_tile,
    Unmolded_tok,
    Incon_meld,
    Extra_meld,
    Reserved_keyword,
  ];
  let severity = o => Option.get(Lists.find_index((==)(o), all));
  let compare = (l, r) => Int.compare(severity(l), severity(r));
};
include Ord;

module Map = Maps.Make(Ord);

let of_token = (tok: Token.t) =>
  switch (tok.mtrl) {
  | Space(White(_)) => None
  | Space(Unmolded) => Some(Unmolded_tok)
  | Grout((_, (Conv, Conv))) => Some(Missing_meld)
  | Grout((_, (Conv, Conc) | (Conc, Conv))) => Some(Incon_meld)
  | Grout((_, (Conc, Conc))) => Some(Extra_meld)
  | Tile((lbl, _)) =>
    let t = Labeler.single(tok.text);
    switch (Option.map(Token.mtrl, t)) {
    | Some(Tile(lbls))
        when
          !Label.is_const(lbl)
          && lbls
          |> List.exists(lbl =>
               Label.is_const(lbl) && Label.is_complete(tok.text, lbl)
             ) =>
      Some(Reserved_keyword)
    | _ when !Label.is_complete(tok.text, lbl) =>
      Some(tok.text == "" ? Missing_tile : Incomplete_tile)
    | _ => None
    };
  };

module Delta = {
  include Map;
  // each value is (# removed, # inserted)
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Map.t((int, int));
  let find = (o, map) => Option.value(find_opt(o, map), ~default=(0, 0));

  let zero = empty;
  let decr = (o, map) =>
    add(o, Tuples.map_fst(n => n + 1, find(o, map)), map);
  let incr = (o, map) =>
    add(o, Tuples.map_snd(n => n + 1, find(o, map)), map);

  let not_hole = (map: t) =>
    snd(find(Missing_tile, map)) > 0
    || snd(find(Incomplete_tile, map)) > 0
    || snd(find(Incon_meld, map)) > 0
    || snd(find(Extra_meld, map)) > 0;

  let compare = (l, r) =>
    Lists.fold_right(
      ~f=
        (o, c) =>
          c != 0
            ? c
            : {
              let (removed_l, inserted_l) = find(o, l);
              let (removed_r, inserted_r) = find(o, r);
              open Compare.Syntax;
              let/ () =
                Int.compare(inserted_l - removed_l, inserted_r - removed_r);
              Int.compare(inserted_l, inserted_r);
            },
      all,
      ~init=0,
    );

  let add_effect =
    fun
    | Effects.Insert(t) =>
      of_token(t) |> Option.map(incr) |> Option.value(~default=Fun.id)
    | Remove(t) =>
      of_token(t) |> Option.map(decr) |> Option.value(~default=Fun.id);
  let of_effects = List.fold_left(Fun.flip(add_effect), zero);

  let minimize =
      (~to_zero=false, ~to_hole=false, f: 'x => option('y), xs: list('x))
      : option('y) => {
    open Options.Syntax;
    let* (y, effs, delta) =
      xs
      |> List.map(Effects.dry_run(f))
      |> List.filter_map(((y_opt, effs)) =>
           y_opt |> Option.map(y => (y, effs, of_effects(effs)))
         )
      |> Lists.min(((_, _, l), (_, _, r)) => compare(l, r));
    if (to_zero && compare(delta, zero) > 0 || to_hole && not_hole(delta)) {
      None;
    } else {
      Effects.commit(effs);
      Some(y);
    };
  };

  let min_choice =
      (~to_zero=false, cs: Choice.t(Options.Thunk.t('x))): option('x) => {
    open Options.Syntax;
    let rec minimize = (c: Choice.t(_)) =>
      switch (c) {
      | Choice.Nil => None
      | One(thunk) =>
        let (y_opt, effs) = Effects.dry_run(f => f(), thunk);
        y_opt |> Option.map(y => (y, effs, of_effects(effs)));
      | Either(l, r) =>
        let (min_l, min_r) = (minimize(l), minimize(r));
        switch (min_l, min_r) {
        | (None, _) => min_r
        | (Some(_), None) => min_l
        | (Some((_, _, delta_l)), Some((_, _, delta_r))) =>
          compare(delta_l, delta_r) <= 0 ? min_l : min_r
        };
      | Prefer(l, r) =>
        switch (minimize(l)) {
        | Some(_) as min_l => min_l
        | None => minimize(r)
        }
      };
    let* (y, effs, delta) = minimize(cs);
    if (to_zero && compare(delta, zero) > 0) {
      None;
    } else {
      Effects.commit(effs);
      Some(y);
    };
  };
};
