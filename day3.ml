open Batteries

let input_file = Aoc.arg_or "day3.txt"

let priority item =
  match Char.is_uppercase item with
  | true -> Char.code item - 38
  | false -> Char.code item - 96

let split2 lst = List.(split_at (length lst / 2) lst)
let intersect2 (l1, l2) = Set.(intersect (of_list l1) (of_list l2) |> any)

(* silver test = 157 *)
let silver =
  File.lines_of input_file
  |> Enum.map String.explode
  |> Enum.map split2
  |> Enum.map intersect2
  |> Enum.map priority
  |> Enum.reduce ( + )

(* https://stackoverflow.com/q/48713886 *)
let group_by_3 lst =
  let accum = ([], [], 0) in
  let f (all_groups, current_group, size) x =
    if size = 3 then (List.rev current_group :: all_groups, [ x ], 1)
    else (all_groups, x :: current_group, size + 1)
  in
  let groups, last, _ = List.fold_left f accum lst in
  List.rev (List.rev last :: groups)

let intersect3 = function
  | [ l1; l2; l3 ] ->
      Set.(intersect (of_list l1) (of_list l2) |> intersect (of_list l3) |> any)
  | _ -> failwith "wrong shape"

(* gold test = 70 *)
let gold =
  File.lines_of input_file
  |> List.of_enum
  |> List.map String.explode
  |> group_by_3
  |> List.map intersect3
  |> List.map priority
  |> List.reduce ( + )

(* let main = Printf.printf "gold:\t%10d\n" silver *)
let main = Printf.printf "silver:\t%10d\ngold:\t%10d\n" silver gold
