open Util;
include Base;

[@deriving show]
type t = piece;

let whitespace = w => Whitespace(w);
let grout = g => Grout(g);
let tile = t => Tile(t);

let get = (f_w, f_g, f_t, p: t) =>
  switch (p) {
  | Whitespace(w) => f_w(w)
  | Grout(g) => f_g(g)
  | Tile(t) => f_t(t)
  };

// let is_balanced =
//   fun
//   | Shard(_) => false
//   | Whitespace(_)
//   | Grout(_)
//   | Tile(_) => true;

let pop = (side: Direction.t, p: t): (t, segment) =>
  switch (p) {
  | Tile(t) => Tile.pop(side, t)
  | Grout(_)
  | Whitespace(_) => (p, [])
  };

let disassemble = (from: Direction.t, p: t): segment =>
  switch (p) {
  | Grout(_)
  | Whitespace(_) => [p]
  | Tile(t) => Tile.disassemble(from, t)
  };

let remold = (p: t) =>
  switch (p) {
  | Grout(_)
  | Whitespace(_) => [p]
  | Tile(t) => List.map(tile, Tile.remold(t))
  };

let shapes =
  get(_ => None, g => Some(Grout.shapes(g)), t => Some(Tile.shapes(t)));

let is_grout: t => bool =
  fun
  | Grout(_) => true
  | _ => false;

let is_whitespace: t => bool =
  fun
  | Whitespace(_) => true
  | _ => false;

let monotile: t => option(string) =
  fun
  | Tile({label: [t], _}) => Some(t)
  | _ => None;

let is_length_one_monotile: t => bool =
  p =>
    switch (monotile(p)) {
    | Some(t) => String.length(t) == 1
    | None => false
    };
