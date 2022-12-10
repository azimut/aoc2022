open! Batteries

(*
  PART 1 = 21 =   1801
  PART 2 =  8 = 209880
*)

let arg_or default =
  match Sys.argv with
  | [| _; "-emacs" |] -> default
  | [| _; filename |] -> filename
  | _ -> default

type forest = { width : int; height : int; mat : int array array }

let parsed_forest =
  let lines = File.lines_of (arg_or "day8.txt") |> List.of_enum in
  let width = lines |> List.first |> String.length in
  let height = List.length lines in
  let mat = Array.make_matrix width height (-1) in
  for h = 0 to height - 1 do
    let line =
      List.nth lines h
      |> String.explode
      |> List.map (String.to_int % String.of_char)
    in
    for w = 0 to width - 1 do
      mat.(h).(w) <- List.at line w
    done
  done;
  { width; height; mat }

let is_visible x y forest =
  let tree = forest.mat.(x).(y) in
  let rec is_visible_from_top h w =
    if h - 1 < 0 then true
    else tree > forest.mat.(h - 1).(w) && is_visible_from_top (h - 1) w
  in
  let rec is_visible_from_bottom h w =
    if h + 1 > forest.height - 1 then true
    else tree > forest.mat.(h + 1).(w) && is_visible_from_bottom (h + 1) w
  in
  let rec is_visible_from_left h w =
    if w + 1 > forest.width - 1 then true
    else tree > forest.mat.(h).(w + 1) && is_visible_from_left h (w + 1)
  in
  let rec is_visible_from_right h w =
    if w - 1 < 0 then true
    else tree > forest.mat.(h).(w - 1) && is_visible_from_right h (w - 1)
  in
  is_visible_from_top x y
  || is_visible_from_bottom x y
  || is_visible_from_left x y
  || is_visible_from_right x y

let silver forest =
  let visibles_trees = ref 0 in
  (* NOTE: Works when is a square *)
  let visibles_in_the_border = (forest.width * 4) - 4 in
  for w = 1 to forest.width - 2 do
    for h = 1 to forest.height - 2 do
      if is_visible h w forest then visibles_trees := !visibles_trees + 1
    done
  done;
  !visibles_trees + visibles_in_the_border

let scenic_score x y forest tree =
  let rec visibility_on_top h w acc =
    if h - 1 < 0 then acc
    else if tree <= forest.mat.(h - 1).(w) then acc + 1
    else visibility_on_top (h - 1) w (acc + 1)
  in
  let rec visibility_on_bottom h w acc =
    if h + 1 > forest.height - 1 then acc
    else if tree <= forest.mat.(h + 1).(w) then acc + 1
    else visibility_on_bottom (h + 1) w (acc + 1)
  in
  let rec visibility_on_left h w acc =
    if w + 1 > forest.width - 1 then acc
    else if tree <= forest.mat.(h).(w + 1) then acc + 1
    else visibility_on_left h (w + 1) (acc + 1)
  in
  let rec visibility_on_right h w acc =
    if w - 1 < 0 then acc
    else if tree <= forest.mat.(h).(w - 1) then acc + 1
    else visibility_on_right h (w - 1) (acc + 1)
  in
  visibility_on_bottom x y 0
  * visibility_on_top x y 0
  * visibility_on_left x y 0
  * visibility_on_right x y 0

let gold forest =
  let max_scenic_score = ref 0 in
  for w = 0 to forest.width - 1 do
    for h = 0 to forest.height - 1 do
      let tree = forest.mat.(h).(w) in
      let score = scenic_score h w forest tree in
      if score > !max_scenic_score then max_scenic_score := score
    done
  done;
  !max_scenic_score

let main = Printf.printf "silver:\t%10d\ngold:\t%10d\n"
             (silver parsed_forest) (gold parsed_forest)
