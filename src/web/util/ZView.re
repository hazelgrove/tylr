open Virtual_dom.Vdom;
open Util;

type t('z) = ZList.t('z, Node.t);

let add = (ls: list(Node.t), rs: list(Node.t), z: t('z)) =>
  ZList.mk(~prefix=z.prefix @ ls, ~z=(), ~suffix=rs @ z.suffix, ());
