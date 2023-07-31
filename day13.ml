open! Batteries

let input_file = "data/day13.test.txt"

let parse = File.with_file_in input_file IO.read_all
          |> String.split_on_string ~by:"\n\n"
          |> List.map (String.split_on_char '\n')


module Parser = struct
  open Angstrom
  let pp =
    let comma = char ',' in
    let lsb = char '[' in
    let rsb = char ']' in
    let num = number >>| fun n -> Num n 
end
