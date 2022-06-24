let debug_update = false;
let debug_keystoke = false;

type entry = {
  update: Update.t,
  error: option(Update.Failure.t),
  timestamp: Model.timestamp,
};

let is_action_logged: Update.t => bool =
  fun
  | UpdateDoubleTap(_) => false
  | Save => false
  | _ => true;

let is_keystroke_logged: Key.t => bool = _ => true;

let mk_entry = (update, error): entry => {
  let error =
    switch (error) {
    | Ok(_) => None
    | Error(failure) => Some(failure)
    };
  {update, error, timestamp: JsUtil.timestamp()};
};

let to_string = (entry: entry) => {
  let status =
    switch (entry.error) {
    | None => "SUCCESS"
    | Some(failure) => "FAILURE(" ++ Update.Failure.show(failure) ++ ")"
    };
  Printf.sprintf(
    "%.0f: %s %s",
    entry.timestamp,
    Update.show(entry.update),
    status,
  );
};

[@deriving show({with_path: false})]
type key_entry = {
  key: Key.t,
  updates: list(Update.t),
  timestamp: Model.timestamp,
};

let mk_key_entry = (key, updates): key_entry => {
  {key, updates, timestamp: JsUtil.timestamp()};
};

let key_entry_to_string = ({key, updates, timestamp}: key_entry) => {
  let updates = updates |> List.map(Update.show) |> String.concat(", ");
  Printf.sprintf("%.0f: %s -> [%s]", timestamp, Key.to_string(key), updates);
};

let update = (update: Update.t, res) => {
  if (is_action_logged(update)) {
    let update_str = to_string(mk_entry(update, res));
    LocalStorage.append_action_log(update_str);
    if (debug_update) {
      print_endline(update_str);
    };
  };
  res;
};

let keystoke = (key: Key.t, updates) => {
  if (is_keystroke_logged(key)) {
    let keystroke_str = key_entry_to_string(mk_key_entry(key, updates));
    LocalStorage.append_keystroke_log(keystroke_str);
    if (debug_keystoke) {
      print_endline(keystroke_str);
    };
  };
  updates;
};
