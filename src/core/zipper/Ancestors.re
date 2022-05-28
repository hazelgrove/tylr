open Util;

[@deriving show]
type generation = (Ancestor.t, Siblings.t);

[@deriving show]
type t = list(generation);

let empty = [];

let sort =
  fun
  | [] => Sort.root
  | [(a, _), ..._] => Ancestor.sort(a);

let zip = (seg: Segment.t, ancs: t) =>
  ancs
  |> List.fold_left(
       (seg, (a, sibs)) =>
         ListFrame.to_list(
           ~subject=[Piece.Tile(Ancestor.zip(seg, a))],
           sibs,
         ),
       seg,
     );

let disassemble = ancs =>
  ancs
  |> List.map(((a, sibs)) =>
       Siblings.concat([Ancestor.disassemble(a), sibs])
     )
  |> Siblings.concat;

let remold = (ancestors: t): list(t) =>
  List.fold_right(
    ((a, sibs), remolded) => {
      open ListUtil.Syntax;
      let+ ancestors = remolded
      and+ sibs = Siblings.remold(sibs)
      and+ a = Ancestor.remold(a);
      [(a, sibs), ...ancestors];
    },
    ancestors,
    [empty],
  );

// let sort_rank = (ancestors: t) =>
//   List.fold_right(
//     ((a, sibs), (s, rank)) => {
//       let rank =
//         rank
//         + Siblings.sort_rank(sibs, s)
//         + Ancestor.sort_rank(a, Siblings.sorts(sibs, s));
//       let s' = Ancestor.sort(a);
//       (s', rank);
//     },
//     ancestors,
//     (Sort.root, 0),
//   )
//   |> snd;
let sort_rank = _ => failwith("todo Ancestors.sort_rank");

let shape_rank = (ancestors: t): int =>
  List.fold_right(
    ((_, sibs), rank) => Siblings.shape_rank(sibs) + rank,
    ancestors,
    0,
  );

let regrout = (ancs: t) =>
  List.fold_right(
    ((a, sibs): generation, regrouted) => {
      open IdGen.Syntax;
      let* regrouted = regrouted;
      let* ((trim_l, l, pre), (trim_r, r, suf)) = Siblings.regrout(sibs);
      let (l', r') = TupleUtil.map2(Nib.shape, Mold.nibs(a.mold));
      let* trim_l = Segment.Trim.regrout((l', l), trim_l);
      let+ trim_r = Segment.Trim.regrout((r', r), trim_r);
      let pre = Segment.Trim.to_seg(trim_l) @ pre;
      let suf = Segment.Trim.to_seg(trim_r) @ suf;
      [(a, (pre, suf)), ...regrouted];
    },
    ancs,
    IdGen.return(empty),
  );
