open Batteries

type hand = Rock | Paper | Scissor

let score_hand = function Rock -> 1 | Paper -> 2 | Scissor -> 3

let score_match = function
  | Rock, Paper | Paper, Scissor | Scissor, Rock -> 6
  | x, y when x = y -> 3
  | _ -> 0

let make_hand = function
  | 'A' | 'X' -> Rock
  | 'B' | 'Y' -> Paper
  | 'C' | 'Z' -> Scissor
  | _ -> failwith "invalid value"

let silver =
  File.lines_of "day2.txt"
  |> Enum.map String.explode
  |> Enum.map (function
       | [ a; _; c ] -> (make_hand a, make_hand c)
       | _ -> failwith "invalid input")
  |> Enum.map (fun (x, y) -> score_match (x, y) + score_hand y)
  |> Enum.reduce ( + )

type result = Lose | Draw | Win

let make_result = function
  | 'X' -> Lose
  | 'Y' -> Draw
  | 'Z' -> Win
  | _ -> failwith "invalid char"

let score_result = function Lose -> 0 | Draw -> 3 | Win -> 6

let hand_from_match = function
  | Rock, Lose | Paper, Win | Scissor, Draw -> Scissor
  | Rock, Win | Paper, Draw | Scissor, Lose -> Paper
  | Rock, Draw | Paper, Lose | Scissor, Win -> Rock

let gold =
  File.lines_of "day2.txt"
  |> Enum.map String.explode
  |> Enum.map (function
       | [ a; _; c ] -> (make_hand a, make_result c)
       | _ -> failwith "bad format")
  |> Enum.map (fun (x, y) ->
         score_result y + score_hand (hand_from_match (x, y)))
  |> Enum.reduce ( + )

let main = Printf.printf "silver: %d\ngold: %d\n" silver gold
