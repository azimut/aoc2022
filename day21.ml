open! Batteries

(* PART 1 = 152 = 31017034894002 *)
(* PART 2 = 301 = *)
type message =
  | Number of int
  | Op of string * string * (int -> int -> int) * (int -> int -> int) * string

type monkey = { id : string; message : message }

let make_monkey (id, raw_message) =
  let message =
    match String.(trim raw_message |> split_on_char ' ') with
    | [ x ] -> Number (Int.of_string x)
    | [ x; "*"; y ] -> Op (x, y, Int.mul, Int.div, "*")
    | [ x; "/"; y ] -> Op (x, y, Int.div, Int.mul, "/")
    | [ x; "+"; y ] -> Op (x, y, Int.add, Int.sub, "+")
    | [ x; "-"; y ] -> Op (x, y, Int.sub, Int.add, "-")
    | _ -> assert false
  in
  { id; message }

let monkeys filename =
  File.lines_of filename
  |> List.of_enum
  |> List.map (String.split ~by:":")
  |> List.map make_monkey

let silver monkeys =
  let open Hashtbl in
  let monkeys_hash = create 2000 in
  let rec loop monkeys =
    match monkeys with
    | [] -> []
    | { id; message = Number value } :: ms ->
        add monkeys_hash id value;
        loop ms
    | { id; message = Op (id1, id2, f, _, _) } :: ms
      when mem monkeys_hash id1 && mem monkeys_hash id2 ->
        add monkeys_hash id (f (find monkeys_hash id1) (find monkeys_hash id2));
        loop ms
    | { id; message } :: ms -> loop (ms @ [ { id; message } ])
  in
  let _ = loop monkeys in
  find monkeys_hash "root"

let root_deps monkeys =
  match List.find (fun m -> m.id = "root") monkeys with
  | { id = _; message = Op (left, right, _, _, _) } -> (left, right)
  | _ -> assert false

let rec solve_root_deps_opt monkeys search_id =
  let next_monkey = List.find (fun m -> m.id = search_id) monkeys in
  match next_monkey with
  | { id = _; message = Number number } -> Some number
  | { id = _; message = Op (i1, i2, _, _, _) } when i2 = "humn" || i1 = "humn"
    ->
      None
  | { id = _; message = Op (i1, i2, f, _, _) } ->
      Option.bind (solve_root_deps_opt monkeys i1) (fun m1 ->
          Option.bind (solve_root_deps_opt monkeys i2) (fun m2 ->
              Some (f m1 m2)))

let solve monkeys total from_id =
  let from_monkey = List.find (fun monkey -> monkey.id = from_id) monkeys in
  match from_monkey with
  | { id = _; message = Number n } -> n
  | { id = _; message = Op (id1, id2, _, f, "-") } -> f total
  | { id = _; message = Op (id1, id2, _, f, _) } -> f total

let gold monkeys =
  let left, right = root_deps monkeys in
  let left_sum, right_sum =
    (solve_root_deps_opt monkeys left, solve_root_deps_opt monkeys right)
  in
  Printf.printf "%s=%d - %s=%d\n" left
    (Option.default 0 left_sum)
    right
    (Option.default 0 right_sum)
