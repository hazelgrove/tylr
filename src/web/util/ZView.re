open Virtual_dom.Vdom;
open Util;

type t('z) = ZList.t('z, Node.t);

let insert = (ls: list(Node.t), rs: list(Node.t), z: t('z)) =>
  ZList.mk(~prefix=z.prefix @ ls, ~z=z.z, ~suffix=rs @ z.suffix, ());

let wrap = (ls, rs, z: t('z)) =>
  ZList.mk(~prefix=ls @ z.prefix, ~z=z.z, ~suffix=z.suffix @ rs, ());
