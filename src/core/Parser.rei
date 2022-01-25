let disassemble_tile: (Util.Direction.t, Tile.t) => Segment.t;

let disassemble_frame: Zipper.Frame.t => (Segment.Frame.t, Zipper.Frame.t);

let assemble_segment: (Util.Direction.t, Segment.t) => Segment.t;

// TODO make sure this parses affixes too
let assemble_zipper:
  (Segment.Frame.t, Zipper.Frame.t) => (Segment.Frame.t, Zipper.Frame.t);

// let convert: (SegmentPath.t, Segment.t) => (TreePath.t, Tree.t);
