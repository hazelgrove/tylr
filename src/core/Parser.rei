open Zipper;

let disassemble_tile: Tile.t => Segment.t;
let reassemble_segment: Segment.t => Segment.t;

// let assemble_affix: (Util.Direction.t, Segment.Affix.t) => Segment.Affix.t;
// let assemble_siblings: Siblings.t => Siblings.t;

let disassemble_ancestor: Ancestor.t => Siblings.t;
// TODO make sure this parses siblings too
let reassemble_relatives:
  (Siblings.t, Ancestors.t) => (Siblings.t, Ancestors.t);

// let convert: (SegmentPath.t, Segment.t) => (TreePath.t, Tree.t);
