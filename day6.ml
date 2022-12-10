open! Batteries

let packet = File.with_file_in (Aoc.arg_or "day6.txt") IO.read_all

let has_no_repeats s n =
  n = (String.explode s |> List.sort_unique Char.compare |> List.length)

let solver uniq_range =
  let open String in
  let marker = ref 0 in
  packet
  |> iteri (fun i _ ->
         if
           length packet > i + uniq_range
           && has_no_repeats (sub packet i uniq_range) uniq_range
           && !marker = 0
         then marker := i + uniq_range);
  marker

let silver = solver 4
let gold = solver 14
