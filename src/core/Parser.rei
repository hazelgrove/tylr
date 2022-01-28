let disassemble_tile: (Util.Direction.t, Tile.t) => Segment.t;

let disassemble_frame:
  Zipper.Frame.t => option((Segment.Frame.t, Zipper.Frame.t));

let assemble_segment: Segment.t => Segment.t;
let assemble_affix: (Util.Direction.t, Segment.Affix.t) => Segment.Affix.t;
let assemble_affixes: Segment.Frame.t => Segment.Frame.t;

// TODO make sure this parses affixes too
let assemble_zipper:
  (Segment.Frame.t, Zipper.Frame.t) => (Segment.Frame.t, Zipper.Frame.t);

// let convert: (SegmentPath.t, Segment.t) => (TreePath.t, Tree.t);
