// TODO rename this module

open Util;

module type S = {
  module T: Tile.S;

  type zoperand;
  type zpreop;
  type zpostop;
  type zbinop;
  type ztile = Tile.t(zoperand, zpreop, zpostop, zbinop);
  type t = ZList.t(option(ztile), T.t);

  type unzipped = option(ztile);
  type zipped = T.s;
  type zipper = (zipped, unzipped);
};
