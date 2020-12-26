// TODO rename this module

open Util;

module type S = {
  module T: Tile.S;

  type zop;
  type zpre;
  type zpost;
  type zbin;
  type ztile = Tile.t(zop, zpre, zpost, zbin);
  type t = ZList.t(option(ztile), T.t);

  type unzipped = option(ztile);
  type zipped = T.s;
  type zipper = (zipped, unzipped);
};
