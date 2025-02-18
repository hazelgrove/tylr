let t0_transcribe = {|let x = 1 in
x + 2|};

let t0_modify = {|let x = 1 in
let y = 2 in
x + y|};

let t1_transcribe = {|fun (center, p) ->
let (x1, y1) = center in
let (x2, y2) = p in
let r = sqrt(pow(x1 - x2, 2) + pow(y1 - y2, 2)) in
circle(center, r)|};

let t1_modify = {|let dist =
fun (p1, p2) ->
let (x1, y1) = p1 in
let (x2, y2) = p2 in
sqrt(pow(x1 - x2, 2) + pow(y1 - y2, 2))
in
fun (center, p) ->
let r = dist(center, p) in
circle(center, r)|};

let t2_transcribe = {|fun (p1, p2) ->
let mark =
fun center ->
let r = 4 in
circle(center, r)
in
[mark(p1), line(p1, p2), mark(p2)]|};

let t2_modify = {|fun (square, p1, p2) ->
let mark =
fun center ->
if square then
let (x, y) = center in
rect(x - 2, y - 2, 4, 4)
else
let r = 4 in
circle(center, r)
in
[mark(p1), line(p1, p2), mark(p2)]|};

let t3_transcribe = {|shapes
|> map(rotate(pi / 4))
|> map(translate(6, 7))
|> filter(fun shape -> area(shape) < 50)
|> map(dilate(5))|};

let t3_modify = {|shapes
|> filter(fun shape -> area(shape) < 50)
|> map(dilate(5))
|> map(rotate(pi / 4))
|> map(translate(6, 7))|};

let t4_transcribe = {|type Point = (Int, Int) in
type Rect = (Point, Int, Int) in

let contains = fun (r: Rect, p: Point) ->
  let (x, y) = p in
  let ((x_min, y_min), x_len, y_len) = r in
  x_min <= x && x <= x_min + x_len
  && y_min <= y && y <= y_min + y_len
in|};

let t4_modify = {|type Point = (Int, Int) in
type Rect = (Point, Int, Int) in
type Circ = (Point, Int) in
type Shape = R(Rect) + C(Circ) in

let contains = fun (s: Shape, p: Point) ->
  let (x, y) = p in
  case s
  | R(((x_min, y_min), x_len, y_len)) =>
 x_min <= x && x <= x_min + x_len
 && y_min <= y && y <= y_min + y_len
  | C((center, r)) => dist(center, p) <= r
in|};

let uncurry_modify = {|let fold_right:
(A -> Acc -> Acc) -> List(A) -> Acc -> Acc = in
fold_right(fun n -> fun sum -> n + sum)(ns)(0)|};

let fuse_modify = {|shapes
|> List.filter(fun s -> area(s) < 100)
|> List.map(fun s ->
let stdDev = if area(s) < 50 then 2 else 4 in
let blurred = blur(stdDev, s) in
blend(blurred, greenRect)
)
|> List.map(skew(4))|};

let emoji_paint = {|type Emoji = None + Smile + Laugh in
let Row = Int in
let Col = Int in
let Grid = [[Emoji]] in

let Model = (
  Grid,
  Emoji,
  [Emoji]
  )
in

let Action =
  StampEmoji(Row, Col)
  + ClearCell(Row, Col)
  + SelectEmoji(Emoji)
in

let init: Model = (
  [
    [None, None, None],
    [None,None,None],
    [None,None,None]
  ],
  Smile,
  [
    Smile,
    Laugh
  ]) in

let updateGrid: (Grid, Row, Col, Emoji) -> Grid =
  fun (grid, row, col, emoji) ->
    List.mapi(
      fun (i, r) ->
        if i == row
        then List.mapi(fun (j, c) -> if j == col then emoji else c, r)
        else r,
     grid
  ) in

let update: (Model, Action) -> Model =
    case action
    | SelectEmoji(emoji) =>
      (grid, emoji, emojiList)
    | StampEmoji(row, col) =>
      (updateGrid(grid, row, col, selectedEmoji), selectedEmoji, emojiList)
    | ClearCell(row, col) =>
      (updateGrid(grid, row, col, None), selectedEmoji, emojiList)
in
update(init, StampEmoji(1, 1))|};

let ep0 = {|type Emoji = None + Smile + Laugh in
type Model = Emoji in
type Action = StampEmoji in

let init: Model = None in

let update: (Model, Action) -> Model =
  fun (model, action) ->
    case action
      | StampEmoji => Smile
in

update(init, StampEmoji)|};

let ep1 = {|type Emoji = None + Smile + Laugh in
type Model = (Emoji, Emoji) in
type Action =
  StampEmoji
  + SelectEmoji(Emoji)
in

let init: Model = (None, Smile)  in

let update: (Model, Action) -> Model =
  fun ((canvas, selectedEmoji), action) ->
    case action
      | SelectEmoji(emoji) =>
      (canvas, emoji)
      | StampEmoji =>
      (selectedEmoji, selectedEmoji)
in update(init, SelectEmoji(Laugh))|};

let ep2 = {|type Emoji = None + Smile + Laugh in
type Model = ([Emoji],Emoji) in
type Action =
  StampEmoji(Int)
  + SelectEmoji(Emoji)
in

let init: Model = ([None, None, None], Smile) in

let update: (Model, Action) -> Model =
  fun ((canvas, selectedEmoji), action) ->
    case action
      | StampEmoji(index) =>
      let new_canvas =
        List.mapi(
          (fun (i, currentEmoji) ->
            if i == index
            then selectedEmoji
          else currentEmoji),
        canvas)
      in (new_canvas, selectedEmoji)
      | SelectEmoji(emoji) =>
      (canvas, emoji)
in
update(init, StampEmoji(2))|};

let ep3 = {|type Emoji = None + Smile + Laugh in
type Model = ([Emoji],Emoji) in
type Action =
  StampEmoji(Int)
  + ClearCell(Int)
  + SelectEmoji(Emoji)
in

let init: Model = ([Smile, None, None], Smile) in

let update_canvas =
  fun (index, emoji, canvas) ->
    List.mapi(
      (fun (i, currentEmoji) ->
        if i == index
        then emoji
      else currentEmoji),
    canvas)
in

let update: (Model, Action) -> Model =
  fun ((canvas, selectedEmoji), action) ->
    case action
      | StampEmoji(index) =>
      (update_canvas(index, selectedEmoji, canvas), selectedEmoji)
      | ClearCell(index) =>
      (update_canvas(index, None, canvas), selectedEmoji)
      | SelectEmoji(emoji) =>
      (canvas, emoji)
in
update(init, ClearCell(0))|};

let ep4 = {|type Emoji = None + Smile + Laugh in
type Model = ([[Emoji]],Emoji) in
type Action =
  StampEmoji(Int, Int)
  + ClearCell(Int, Int)
  + SelectEmoji(Emoji)
in

let init: Model = (
  [
    [None, None, None],
    [None, None, None],
    [None, None, None]
  ],
Smile)
in

let update_row =
  fun (target_col, emoji, row) ->
    List.mapi(
      (fun (col_index, col) ->
      if col_index == target_col then emoji else col),
row) in

let update_canvas =
  fun (target_row, target_col, emoji, canvas) ->
    List.mapi(
      (fun (row_index, row) ->
        if row_index == target_row
        then update_row(target_col, emoji, row)
      else row),
    canvas)
in

let update: (Model, Action) -> Model =
  fun ((canvas, selectedEmoji), action) ->
    case action
      | StampEmoji(row, col) =>
      (update_canvas(row, col, selectedEmoji, canvas), selectedEmoji)
      | ClearCell(row, col) =>
      (update_canvas(row, col, None, canvas), selectedEmoji)
      | SelectEmoji(emoji) =>
      (canvas, emoji)
in
update(init, StampEmoji(1, 1)) |};

let epzz = {|type Model = in
type Action = in

let init: Model = TODO in

let update: (Model, Action) -> Model =
  fun (model, action) ->
    case action
    | => TODO
in
update(init, )|};

let epz0 = {|type Emoji = Smile + Frown + Smirk in
type Cell = Empty + Stamped(Emoji) in
type Model = Cell in
type Action = StampEmoji in

let init: Model = None in

let update: (Model, Action) -> Model =
  fun (_cell, action) ->
    case action
    | StampEmoji => Smile
in

update(init, StampEmoji)|};

let epz1 = {|type Emoji = Smile + Frown + Smirk in
type Cell = Empty + Stamped(Emoji) in
type Model = [Cell] in
type Action = StampEmoji(Int) in

let init: Model = [None, None, None] in

let update: (Model, Action) -> Model =
  fun (cells, action) ->
    case action
    | StampEmoji(index) => update_nth(index, Stamped(Smile), cells)
in

update(init, StampEmoji)|};

let epz2 = {|type Emoji = Smile + Frown + Smirk in
type Cell = Empty + Stamped(Emoji) in
type Model = ([Cell], Emoji) in
type Action = StampEmoji(Int) + SelectEmoji(Emoji) in

let init: Model = ([None, None, None], Smile) in

let update: (Model, Action) -> Model =
  fun ((cells, selected), action) ->
    case action
    | StampEmoji(index) =>
      (update_nth(index, Stamped(Smile), cells), selected)
    | SelectEmoji(new) => (cells, new)
in

update(init, StampEmoji)|};

let epz3 = {|type Emoji = Smile + Frown + Smirk in
type Cell = Empty + Stamped(Emoji) in
type Model = ([Cell], Emoji) in
type Action =
  StampEmoji(Int)
  + ClearCell(Int)
  + SelectEmoji(Emoji)
in

let init: Model = ([None, None, None], Smile) in

let update: (Model, Action) -> Model =
  fun ((cells, selected), action) ->
    case action
    | StampEmoji(index) =>
      (update_nth(index, Stamped(Smile), cells), selected)
    | ClearCell(index) =>
      (update_nth(index, Empty, cells), selected)
    | SelectEmoji(new) => (cells, new)
in

update(init, StampEmoji)|};

let epz4 = {|type Emoji = Smile + Frown + Smirk in
type Cell = Empty + Stamped(Emoji) in
type Model = ([[Cell]], Emoji) in
type Action =
  StampEmoji(Int, Int)
  + ClearCell(Int, Int)
  + SelectEmoji(Emoji)
in

let init: Model = (
  [[None, None, None],
   [None, None, None],
   [None, None, None]],
  Smile)
in

let update_grid: (Int, Int, Cell, [[Cell]]) -> [[Cell]] =
  fun (row, col, cell, grid) ->
    update_nth(row, update_nth(col, cell, List.nth(row, grid)), grid)
in

let update: (Model, Action) -> Model =
  fun ((cells, selected), action) ->
    case action
    | StampEmoji(row, col) =>
      (update_grid(row, col, Stamped(Smile), cells), selected)
    | ClearCell(row, col) =>
      (update_grid(row, col, Empty, cells), selected)
    | SelectEmoji(new) => (cells, new)
in

update(init, StampEmoji)


|};
