open! Batteries

let arg_or default =
  match Sys.argv with
  | [| _; "-emacs" |] -> default
  | [| _; filename |] -> filename
  | _ -> default

let input_file = arg_or "day5.txt"

type movement = { qty : int; src : int; dst : int }

let movements =
  File.with_file_in input_file IO.read_all
  |> String.split_on_string ~by:"\n\n"
  |> List.last
  |> String.trim
  |> String.split_on_char '\n'
  |> List.map (fun s ->
         Scanf.sscanf s "move %d from %d to %d" (fun qty src dst ->
             { qty; src = src - 1; dst = dst - 1 }))

let input_stacks file =
  let lines =
    File.with_file_in file IO.read_all
    |> String.split_on_string ~by:"\n\n"
    |> List.first
    |> String.split_on_char '\n'
    |> List.rev
  in
  let stacks =
    List.init
      ((String.length (List.first lines) / 4) + 1)
      (fun _ -> Stack.create ())
  in
  lines
  |> List.iter
       (String.iteri (fun i c ->
            if Char.is_letter c then Stack.push c (List.at stacks (i / 4))));
  stacks

let popper stack n =
  let ret = ref [] in
  for i = 0 to n - 1 do
    ret := Stack.pop stack :: !ret
  done;
  !ret

let pusher stack = List.iter (fun e -> Stack.push e stack)

let silver =
  let stacks = input_stacks input_file in
  movements
  |> List.iter (fun m ->
         let src_stack = List.at stacks m.src in
         let dst_stack = List.at stacks m.dst in
         let to_move = popper src_stack m.qty |> List.rev in
         pusher dst_stack to_move);
  stacks
  |> List.map (fun s -> if Stack.length s = 0 then ' ' else Stack.top s)
  |> String.implode

let gold =
  let stacks = input_stacks input_file in
  movements
  |> List.iter (fun m ->
         let src_stack = List.at stacks m.src in
         let dst_stack = List.at stacks m.dst in
         let to_move = popper src_stack m.qty in
         pusher dst_stack to_move);
  stacks
  |> List.map (fun s -> if Stack.length s = 0 then ' ' else Stack.top s)
  |> String.implode

let _ = Printf.printf "silver:\t%s\ngold:\t%s\n" silver gold
