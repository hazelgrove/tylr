type entry = {
  action: Update.t,
  error: Update.Result.t(Model.t),
  timestamp: Model.timestamp,
};

type t = list(entry);

let is_logged: Update.t => bool =
  fun
  | UpdateDoubleTap(_) => false
  | Save => false
  | _ => true;

let mk_entry = (action, error): entry => {
  {action, error, timestamp: JsUtil.timestamp()};
};

let to_string = (entry: entry) => {
  let status =
    switch (entry.error) {
    | Ok(_) => "SUCCESS"
    | Error(error) => "FAILURE(" ++ Update.Failure.show(error) ++ ")"
    };
  Printf.sprintf(
    "%.0f: %s %s",
    entry.timestamp,
    Update.show(entry.action),
    status,
  );
};
