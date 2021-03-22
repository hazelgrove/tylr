module type S = {
  module Tm: Term.S;
  module T: Tile.S with module Tm := Tm;
  module F: Frame.S with module Tm := Tm;

  [@deriving sexp]
  type t = (Subject.t(T.t), F.bidelimited);
  [@deriving sexp]
  type pointing = (Subject.pointing(T.t), F.bidelimited);
  [@deriving sexp]
  type selecting = (Subject.selecting(T.t), F.bidelimited);
  [@deriving sexp]
  type restructuring = (Subject.restructuring(T.t), F.bidelimited);
};

/*
   let rec zip_to_nearest_bidelim =-
           (zipped: Zipped.tiles, unzipped: Unzipped.t): I.t => {
     switch (unzipped) {
     | Bidelimited(bidelim) => (zipped, bidelim)
     | Pre_r(pre, unzipped) =>
       zip_to_nearest_bidelim([Tile.Pre(pre), ...zipped], unzipped)
     | Post_l(unzipped, post) =>
       zip_to_nearest_bidelim(zipped @ [Tile.Post(post)], unzipped)
     | Bin_l(unzipped, bin, r) =>
       zip_to_nearest_bidelim(
         zipped @ [Tile.Bin(bin), ...HExp.to_htiles(r)],
         unzipped,
       )
     | Bin_r(l, bin, unzipped) =>
       zip_to_nearest_bidelim(
         HExp.to_htiles(l) @ [Tile.Bin(bin), ...zipped],
         unzipped,
       )
     };
   };
 */
