open! Batteries
(* PART 1 = 152 = 31017034894002 *)
(* PART 2 = 301 = *)
type message  =
  | Number of int
  | Op of string * string * (int->int->int)

type monkey = { id: string; message: message }

let make_monkey (id,raw_message) =
  let message = match String.(trim raw_message |> split_on_char ' ') with
    | [x]       -> Number (Int.of_string x)
    | [x;"*";y] -> Op (x, y, Int.mul)
    | [x;"/";y] -> Op (x, y, Int.div)
    | [x;"+";y] -> Op (x, y, Int.add)
    | [x;"-";y] -> Op (x, y, Int.sub)
    | _         -> assert false
  in
  {id;message}

let monkeys filename =
  File.lines_of filename
  |> List.of_enum
  |> List.map (String.split ~by:":")
  |> List.map (make_monkey)

let silver monkeys =
  let open Hashtbl in
  let monkeys_hash = create 2000 in
  let rec loop monkeys =
    match monkeys with
    | [] -> []
    | {id;message=Number(value)} :: ms ->
       add monkeys_hash id value;
       loop ms
    | {id;message=Op(id1,id2,f)} :: ms when mem monkeys_hash id1 && mem monkeys_hash id2 ->
       add monkeys_hash id (f (find monkeys_hash id1) (find monkeys_hash id2));
       loop ms
    | {id;message} :: ms ->
       loop (ms@[{id;message}])
  in
  let _ = loop monkeys in
  find monkeys_hash "root"

let root_deps monkeys =
  match List.find (fun m -> m.id = "root") monkeys with
    | {id=_;message=Op(left,right,_)} -> left,right
    | _                               -> assert false

let rec solve_non_humn_opt monkeys search_id =
  let next_monkey = List.find (fun m -> m.id = search_id) monkeys in
  match next_monkey with
  | {id=_;message=Number(number)} -> Some number
  | {id=_;message=Op(i1,i2,_)} when i2 = "humn" || i1 = "humn" -> None
  | {id=_;message=Op(i1,i2,f)}    ->
     Option.bind (solve_non_humn_opt monkeys i1) (fun m1 ->
       Option.bind (solve_non_humn_opt monkeys i2) (fun m2 ->
           Some(f m1 m2)))

let gold monkeys =
  let dep1,dep2 = root_deps monkeys in
  let sum1, sum2 = solve_non_humn_opt monkeys dep1, solve_non_humn_opt monkeys dep2 in
  let found = ref false in
  let tmp = ref 0 in
  while !found do
    incr tmp;
    found := true
  done;
  Printf.printf "%s=%d - %s=%d\n" dep1 (Option.default 0 sum1) dep2 (Option.default 0 sum2)
