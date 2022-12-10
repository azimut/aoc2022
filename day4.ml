open Batteries

let input_file = Aoc.arg_or "day4.txt"

let range_to_section lst =
  let section = BitSet.create 100 in
  List.iter (fun n -> BitSet.set section n) lst;
  section

let does_one_fully_overlap a b =
  BitSet.(count (diff a b) = 0 || count (diff b a) = 0)

let parsed =
  File.lines_of input_file
  |> Enum.map (String.split_on_char ',')
  |> Enum.map (List.map (String.split_on_char '-'))
  |> Enum.map (List.map (List.map String.to_int))
  |> Enum.map
       (List.map (fun [ a; b ] -> List.range a `To b |> range_to_section))

(* silver = 2 - 444*)
let silver =
  parsed
  |> Enum.clone
  |> Enum.map (fun [ elf1; elf2 ] -> does_one_fully_overlap elf1 elf2)
  |> Enum.filter Fun.id
  |> Enum.count

let do_overlap a b =
  let tmp = BitSet.copy a in
  BitSet.differentiate a b;
  BitSet.equal a tmp

(* gold = 4  - 801*)
let gold =
  parsed
  |> Enum.map (fun [ elf1; elf2 ] -> do_overlap elf1 elf2)
  |> Enum.filter not
  |> Enum.count

let main = Aoc.print_results silver gold
