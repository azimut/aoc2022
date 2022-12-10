open! Batteries

type op = Noop | Add of int
type cpu = { cycle : int; x : int }

let ops =
  let f line =
    if String.equal line "noop" then Noop
    else Scanf.sscanf line "addx %d" (fun d -> Add d)
  in
  File.lines_of "day10.txt" |> Enum.map f |> List.of_enum
