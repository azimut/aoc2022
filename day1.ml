open Batteries

let input =
  File.with_file_in "data/day1.txt" IO.read_all
  |> String.split_on_string ~by:"\n\n"
  |> List.map (String.split_on_char '\n' % String.trim)
  |> List.map (List.map int_of_string)
  |> List.map (List.reduce ( + ))

let silver = input |> List.max

let gold =
  input |> List.sort compare |> List.rev |> List.take 3 |> List.reduce ( + )

let main = Printf.printf "silver: %d gold: %d\n" silver gold
