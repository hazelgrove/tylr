// left step identifies tile,
// right step identifies token
// or bidelimited opseq
type steps = list((int, int));

type t = (steps, Side.t);

/*
 let of_z: ZExp.t => t =
   fun
   | (z', side) => (of_z'(z), side)
 and of_z': ZExp.t' => t' =
   fun
   | (_, ZOperand(zoperand, (prefix, _))) =>
     [Seq.affix_length(prefix), ...of_zoperand(zoperand)]
   | (_, ZPre(zpre, (prefix, _))) =>
     [Seq.affix_length(prefix), ...of_zpre(zpre)]
   | (_, ZPost(_)) => .
   | (_, ZBinOp(zbinop, (prefix, _))) =>
     [Seq.seq_length(prefix), ...of_zbinop(zbinop)]
 and of_zoperand: ZExp.zoperand => (int, t') =
   fun
   | HoleZ
   | NumZ(_)
   | ParenZ_open(_) =>  (0, [])
   | ParenZ_body(z') => (1, of_z'(z'))
   | ParenZ_close(_) => (2, [])
 and of_zpre: ZExp.pre => (int, t') =
   fun
   | IfZ_if(_) => (0, [])
   | IfZ_cond(z', _) => (1, of_z'(z'))
   | IfZ_then(_) => (2, [])
   | IfZ_then_clause(_, z') => (3, of_z'(z'))
   | IfZ_else(_) => (4, [])
 and of_zbinop: ZExp.zbinop => (int, t') =
   fun
   | PlusZ
   | TimesZ
   | EqZ => (0, []);
 */

// steps concluding with tile index
// (similar to char index)
type tile_path = (steps, int);

let cons' = (left_right, (steps, tile_index): tile_path) => (
  [left_right, ...steps],
  tile_index,
);

/*
 let tile_path_of_z = (ze: ZExp.t): target_path =>
   switch (ze) {
   | ((_, ZOperand(HoleZ | NumZ(_), (prefix, _))), side)
   | ((_, ZOperand(ParenZ_open(_), (prefix, _))), Before as side)
   | ((_, ZOperand(ParenZ_close(_), (prefix, _))), After as side) =>
     let tile_index = {
       let prefix_length = Seq.affix_length(prefix);
       switch (side) {
       | Before => prefix_length
       | After => prefix_length + 1
       };
     };
     ([], tile_index);
   | ((_, ZOperand(ParenZ_open(_), (prefix, _))), After) =>
     let left_step = Seq.affix_length(prefix);
     ([(left_step, 1)], 0);
   | ((_, ZOperand(ParenZ_close(_), (prefix, suffix))), Before) =>
     let left_step = Seq.affix_length(prefix);
     let tile_index = Seq.affix_length(prefix) + 1 + Seq.affix_length(suffix);
     ([(left_step, 1)], tile_index);
   | ((_, ZOperand(ParenZ_body(ze'), (prefix, _))), side) =>
     let left_step = Seq.affix_length(prefix);
     cons'((left_step, 1), target_path_of_z((ze', side)));

   | (_, ZPre(zpre, (prefix, _)), side) =>
     let prefix_length = Seq.affix_length(prefix);
     switch (zpre, side) {
     | (IfZ_if(_), Before) => ([], prefix_length)
     | (IfZ_if(_), After) => ([(prefix_length, 1)], 0)
     | (IfZ_cond(ze', _, _), _) =>
       cons'((prefix_length, 1), tile_path_of_z(ze'))
     | (IfZ_then((_, seq), _), Before) =>
       ([(prefix_length, 1)], Seq.seq_length(seq))
     | (IfZ_then(_), After) =>
       ([(prefix_length, 3)], 0)
     | (IfZ_then_clause(_, ze'), _) =>
       cons'((prefix_length, 3), tile_path_of_z(ze'))
     | (IfZ_else(_, (_, seq)), Before) =>
       ([(prefix_length, 3)], Seq.seq_length(seq))
     | (IfZ_else(_), After) => ([], prefix_length + 1)
     };

   | (_, ZPost(_)) => .

   | (_, ZBinOp(zbinop, (prefix, _))) =>
     let left_step = Seq.seq_length(prefix);
     let (right_step, path) = tile_path_of_zbinop((zbinop, side));
     cons'((left_step, right_step), path);
   }
 and tile_path_of_zoperand = ((zoperand, side): (ZExp.zoperand, Side.t)): (int, t') =
   fun
   | HoleZ
   | NumZ(_)
   | ParenZ_open(_) =>  (0, [])
   | ParenZ_body(z') => (1, of_z'(z'))
   | ParenZ_close(_) => (2, [])
 and tile_path_of_zpre: ZExp.pre => (int, t') =
   fun
   | IfZ_if(_) => (0, [])
   | IfZ_cond(z', _) => (1, of_z'(z'))
   | IfZ_then(_) => (2, [])
   | IfZ_then_clause(_, z') => (3, of_z'(z'))
   | IfZ_else(_) => (4, [])
 and tile_path_of_zbinop: ZExp.zbinop => (int, t') =
   fun
   | PlusZ
   | TimesZ
   | EqZ => (0, []);
 */

let get_subexp_of_tile =
    (right_step: int, tile: UHExp.tile)
    : option((UHExp.t => UHExp.tile, UHExp.t)) =>
  switch (right_step, tile) {
  | (1, Operand(Paren(e))) => Some(((e => Operand(Paren(e))), e))
  | (1, PreOp(If(e1, e2))) => Some(((e1 => PreOp(If(e1, e2))), e1))
  | (3, PreOp(If(e1, e2))) => Some(((e2 => PreOp(If(e1, e2))), e2))
  | (3, PreOp(Let(x, def))) => Some(((def => PreOp(Let(x, def))), def))
  | _ => None
  };

let rec get_subexp =
        (steps: steps, e: UHExp.t): option((UHExp.t => UHExp.t, UHExp.t)) =>
  switch (steps, e) {
  | ([], _) => Some(((e => e), e))
  | ([(left_step, right_step), ...steps], (skel, tiles)) =>
    let (prefix, tile, suffix) = Tiles.split_nth(left_step, tiles);
    switch (get_subexp_of_tile(right_step, tile)) {
    | None => None
    | Some((tile_wrapper, subexp)) =>
      switch (get_subexp(steps, subexp)) {
      | None => None
      | Some((wrapper, subexp)) =>
        Some((
          (
            subexp => (
              skel,
              prefix @ [tile_wrapper(wrapper(subexp)), ...suffix],
            )
          ),
          subexp,
        ))
      }
    };
  };

let rec is_before = ((steps1, index1), (steps2, index2)): bool =>
  switch (steps1, steps2) {
  | ([], []) => index1 < index2
  | ([(left1, _), ..._], []) => left1 < index2
  | ([], [(left2, _), ..._]) => index1 <= left2
  | ([(left1, right1), ...steps1], [(left2, right2), ...steps2]) =>
    left1 < left2
    || left1 == left2
    && right1 < right2
    || left1 == left2
    && right1 == right2
    && is_before((steps1, index1), (steps2, index2))
  };
