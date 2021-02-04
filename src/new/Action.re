[@deriving sexp]
type t =
  | Mark
  | Move(Direction.t)
  | Delete(Direction.t)
  | Construct(HTile.shape);

module Make =
       (
         Term: HTerm.S,
         Frame: HFrame.S,
         Z: Zipper.S with module Term := Term and module Frame := Frame,
         I: {
           let mk: Z.t => Zipper.t;

           let move_into_root:
             (Direction.t, Term.t, Frame.t) => option(Zipper.t);
           let move_into_frame:
             (Direction.t, Term.t, Frame.bidelimited) => option(Zipper.t);
         },
       ) => {
  let move_into_tile =
      (
        d: Direction.t,
        {prefix, z, suffix}: ZList.t(Term.tile, Term.tile),
        frame: Frame.t,
      )
      : option(Zipper.t) => {
    let n = List.length(prefix);
    let ts = prefix @ [z, ...suffix];
    let rec go = (skel: Skel.t, frame: Frame.t) => {
      let t = List.nth(ts, Skel.root_index(skel));
      let move_into_root = () =>
        I.move_into_root(d, Term.of_skel(skel, ts), frame);
      switch (skel) {
      | Op(m) =>
        assert(n == m);
        move_into_root();
      | Pre(m, r) =>
        n == m ? move_into_root() : go(r, Pre_r(Tile.get_pre(t), frame))
      | Post(l, m) =>
        n == m ? move_into_root() : go(l, Post_l(frame, Tile.get_post(t)))
      | Bin(l, m, r) =>
        let bin = Tile.get_bin(t);
        if (n < m) {
          go(l, Bin_l(frame, bin, r));
        } else if (n > m) {
          go(r, Bin_r(l, bin, frame));
        } else {
          move_into_root();
        };
      };
    };
    go(HExp.associate(ts), frame);
  };

  let perform_pointing =
      (
        a: Action.t,
        {prefix, z: (), suffix} as pointing: Z.Subject.pointing,
        frame: Frame.bidelimited,
      )
      : option(Zipper.t) =>
    switch (a) {
    | Mark =>
      let selecting = {
        prefix: List.map(Either.l, prefix),
        z: (Left, []),
        suffix: List.map(Either.l, suffix),
      };
      Some(I.mk((Selecting(selecting), frame)));
    | Move(d) =>
      let j = List.length(prefix);
      let exit = () => {
        let+ tile = frame;
        // TODO first try moving to next child
        I.move_into_frame(d, Term.of_tiles(prefix @ suffix), tile);
      };
      switch (d) {
      | Left when j == 0 => exit()
      | Right when j == List.length(zipped) => exit()
      | _ =>
        let zipped = prefix @ suffix;
        let n = d == Left ? j - 1 : j;
        switch (
          move_into_tile(d, ZList.split_at(n, zipped), Bidelimited(frame))
        ) {
        | Some(_) as entered => entered
        | None =>
          let j = d == Left ? j - 1 : j + 1;
          let (prefix, suffix) = ListUtil.split_n(j, zipped);
          Some(I.mk((Pointing({prefix, z: (), suffix}), frame)));
        };
      };
    | Delete(_)
    | Construct(_) => failwith("todo")
    };

  let perform_selecting =
      (
        a: Action.t,
        {prefix, z: (side, selection), suffix} as selecting,
        frame,
      ) =>
    switch (a) {
    | Mark =>
      let upgrade =
        List.map(
          fun
          | L(tile) => L(tile)
          | R(tessera) => R([tessera]),
        );
      let restructuring = {
        prefix: upgrade(prefix),
        z: (side, [selection]),
        suffix: upgrade(suffix),
      };
      Some(I.mk((Restructuring(restructuring), frame)));

    | Move(d) =>
      let moved_within_zipped = (~prefix=prefix, ~suffix=suffix, selection) =>
        Some(
          I.mk((Selecting({prefix, z: (side, selection), suffix}), frame)),
        );
      if (d != side) {
        if (selection == []) {
          move_selecting(d, {prefix, z: (d, selection), suffix}, frame);
        } else {
          switch (side) {
          | Left =>
            let (first, trailing) = ListUtil.split_first(selection);
            switch (first) {
            | R(_) =>
              let prefix = Term.parse_selection(prefix @ [first]);
              moved_within_zipped(~prefix, trailing);
            | L(tile) =>
              let (open_, body, close) = HTile.flatten_tile(tile);
              let prefix = prefix @ [R(open_)];
              let selection = body @ [R(close), ...trailing];
              moved_within_zipped(~prefix, selection);
            };
          | Right =>
            let (leading, last) = ListUtil.split_last(selection);
            switch (last) {
            | R(_) =>
              let suffix = Term.parse_selection([last, ...suffix]);
              moved_within_zipped(~suffix, selection);
            | L(tile) =>
              let (open_, body, close) = HTile.flatten_tile(tile);
              let suffix = [R(close), ...suffix];
              let selection = leading @ [R(open_), ...body];
              moved_within_zipped(~suffix, selection);
            };
          };
        };
      } else {
        switch (side) {
        | Left =>
          switch (ListUtil.split_last_opt(prefix)) {
          | None => failwith("todo: move into frame")
          | Some((leading, R(tessera))) =>
            let selection = Term.parse_selection([R(tessera), ...selection]);
            moved_within_zipped(~suffix=leading, selection);
          | Some((leading, L(tile))) =>
            let (open_, body, close) = T.flatten_tile(tile);
            let prefix =
              prefix
              @ [R(Open(open_)), ...List.map(Either.l, T.flatten(body))];
            let selection = [R(Close(close)), ...selection];
            moved_within_zipped(~prefix, selection);
          }
        | Right =>
          switch (suffix) {
          | [] => failwith("todo: move into frame")
          | [R(tessera), ...trailing] =>
            let selection = Term.parse_selection(selection @ [R(tessera)]);
            moved_within_zipped(~suffix=trailing, selection);
          | [L(tile), ...trailing] =>
            let (open_, body, close) = T.flatten_tile(tile);
            let suffix =
              List.map(Either.l, T.flatten(body))
              @ [R(Close(close)), ...suffix];
            let selection = selection @ [R(Open(open_))];
            moved_within_zipped(~suffix, selection);
          }
        };
      };

    | Delete(_)
    | Construct(_) => failwith("todo")
    };

  let perform_restructuring =
      (
        a: Action.t,
        {prefix, z, suffix}: Z.Subject.restructuring,
        frame: Frame.bidelimited,
      )
      : option(Zipper.t) =>
    switch (a) {
    | Move(d) =>
      let (side, selections) = z;
      let picked_up_selection = (~prefix=prefix, ~suffix=suffix, selections) =>
        Some(I.mk((Restructuring({prefix, z: selections, suffix}), frame)));
      let picked_up_all_selections =
        List.for_all(Either.is_L, prefix)
        && List.for_all(Either.is_L, suffix);
      let move_through_tile = (prefix, tile, suffix) =>
        if (picked_up_all_selections) {
          let prefix = List.filter_map(Either.get_L, prefix);
          let suffix = List.filter_map(Either.get_L, suffix);
          let+ pointing = move_pointing(d, {prefix, z: (), suffix}, frame);
          G.restructuring_of_pointing(pointing);
        } else {
          let (prefix, suffix) =
            switch (d) {
            | Left => (prefix, [tile, ...suffix])
            | Right => (prefix @ [tile], suffix)
            };
          Some(I.mk_g({prefix, z, suffix}, frame));
        };
      switch (d) {
      | Left =>
        switch (ListUtil.split_last_opt(prefix)) {
        | None => failwith("todo")
        | Some((leading, L(_) as tile)) =>
          move_through_tile(leading, tile, suffix)
        | Some((leading, R(_) as selection)) =>
          picked_up_selection(
            ~prefix=leading,
            (Left, [selection, ...selections]),
          )
        }
      | Right =>
        switch (suffix) {
        | [] => failwith("todo")
        | [L(_) as tile, ...trailing] =>
          move_through_tile(prefix, tile, suffix)
        | [R(selection), ...trailing] =>
          picked_up_selection(
            ~suffix=trailing,
            (Right, selections @ [selection]),
          )
        }
      };

    | Mark
    | Delete(_)
    | Construct(_) => failwith("todo")
    };

  let perform = (a: t, (subject, frame): Z.t): option(Zipper.t) =>
    switch (subject) {
    | Normal(normal) => perform_normal(a, subject, frame)
    | Selecting(selecting) => perform_selecting(a, selecting, frame)
    | Restructuring(restructuring) =>
      perform_restructuring(a, restructuring, frame)
    };
};

module Exp = {
  module Make_input = {
    let mk = z => Zipper.Exp(z);

    let move_into_root = (d: Direction.t, subject: HExp.t, frame: Frame.Exp.t) => {
      let mk_normal = tiles =>
        switch (d) {
        | Left => ZList.mk(~prefix=tiles, ~z=(), ())
        | Right => ZList.mk(~z=(), ~suffix=tiles, ())
        };
      subject
      |> HTerm.get(
           fun
           | OpHole
           | Num(_)
           | Var(_) => None
           | Paren(body) => {
               let subject = mk_normal(HExp.flatten(body));
               let frame = Some(Frame.Exp.Paren_body(frame));
               Some(Zipper.Exp((subject, frame)));
             },
           fun
           | Lam(p) => {
               let subject = mk_normal(HPat.flatten(body));
               let frame = Some(Frame.Pat.Lam_pat(frame));
               Some(Zipper.Pat((subject, frame)));
             }
           | Let(p, def) =>
             switch (d) {
             | Left =>
               let subject = mk_normal(HExp.flatten(def));
               let frame = Some(Frame.Exp.Let_def(frame));
               Some(Zipper.Exp((subject, frame)));
             | Right =>
               let subject = mk_normal(HPat.flatten(p));
               let frame = Some(Frame.Pat.Let_pat(frame));
               Some(Zipper.Pat((subject, frame)));
             },
           fun
           | Ap(_) => failwith("todo"),
           fun
           | Plus
           | BinHole => None,
         );
    };

    let move_into_frame =
        (d: Direction.t, subject: HExp.t, frame: Frame.Exp.bidelimited) => {
      open Term.Exp;
      open Frame.Exp;
      let+ tile = frame;
      tile
      |> Tile.get(
           fun
           | Paren_body(frame) => {
               let subject = HTerm.Op(Paren(subject));
               let zipped =
                 Zipper.Exp.zip_to_nearest_bidelimited_frame(subject, frame);
               Zipper.Exp(zipped);
             },
           fun
           | Let_def(p, frame, body) => {
               let subject = HTerm.Pre(Let(p, subject), body);
               let zipped =
                 Zipper.Exp.zip_to_nearest_bidelimited_frame(subject, frame);
               Zipper.Exp(zipped);
             },
           fun
           | Ap_arg(_) => failwith("todo"),
           fun
           | () => raise(Void_bin),
         );
    };
  };
  include Make(Term.Exp, Frame.Exp, Zipper.Exp, Make_input);
};

let perform = (a: t, zipper: Zipper.t): option(Zipper.t) =>
  switch (zipper) {
  | `Typ(zipper) => Typ.perform(a, zipper)
  | `Pat(zipper) => Pat.perform(a, zipper)
  | `Exp(zipper) => Exp.perform(a, zipper)
  };
