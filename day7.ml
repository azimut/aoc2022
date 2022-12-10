open! Batteries

(*
    PART 1 = 1118405
    PART 2 = 12545514
*)

type dirtree = File of string * int | Dir of string * dirtree list * int

(* NOTE: day7.txt has repeated directory names *)
(* NOTE: this works because I know logs cd's to something meaninful before ls *)
let parsed =
  File.with_file_in (Aoc.arg_or "day8.txt") IO.read_all
  |> String.trim
  |> String.split_on_char '\n'
  |> List.filter (( <> ) "$ ls")

let rec map f = function
  | Dir (dirname, x :: xs, size) ->
      f (Dir (dirname, map f x :: List.map (fun x -> map f x) xs, size))
  | Dir (_, [], _) as dir -> f dir
  | File _ as file -> f file

let rec fold f acc = function
  | Dir (_, x :: xs, _) as dir ->
      f (fold f (List.fold_left (fold f) acc xs) x) dir
  | Dir (_, [], _) as dir -> f acc dir
  | File _ as file -> f acc file

let insert' at_dir new_dt = function
  | Dir (name, subpaths, size) when name = at_dir ->
      Dir (name, new_dt :: subpaths, size)
  | Dir _ as dir -> dir
  | File _ as file -> file

let insert at_dir new_dt = map (insert' at_dir new_dt)

module Cwd = struct
  type t = string Stack.t

  let create = Stack.create
  let pushd = Stack.push
  let popd = Stack.pop

  let pwd cwd =
    Stack.enum cwd |> List.of_enum |> List.rev |> String.join "/" |> function
    | "/" -> "/"
    | s -> String.lchop s

  let ls cwd path =
    match pwd cwd with
    | "/" -> Printf.sprintf "/%s" path
    | s -> Printf.sprintf "%s/%s" s path
end

let directory_tree =
  let sum_filesizes accumulator = function
    | File (_, size) -> size + accumulator
    | Dir _ -> accumulator
  in
  let update_dir_size' = function
    | Dir (name, subdirs, _) as dir ->
        Dir (name, subdirs, fold sum_filesizes 0 dir)
    | File _ as f -> f
  in
  let init_immediate_dirsizes = map update_dir_size' in
  let cwd = Cwd.create () in
  let read_cd s = Scanf.sscanf s "$ cd %s" Fun.id in
  let read_dir cwd s =
    Dir (Scanf.sscanf s "dir %s" (fun s -> Cwd.ls cwd s), [], 0)
  in
  let read_file s =
    Scanf.sscanf s "%d %s" (fun size name -> File (name, size))
  in
  let rec silver' cwd dt lst =
    match lst with
    | "$ cd /" :: xs ->
        Cwd.pushd "/" cwd;
        silver' cwd dt xs
    | "$ cd .." :: xs ->
        let _ = Cwd.popd cwd in
        silver' cwd dt xs
    | cd :: xs when String.starts_with cd "$ cd" ->
        let dir = read_cd cd in
        Cwd.pushd dir cwd;
        silver' cwd dt xs
    | dir :: xs when String.starts_with dir "dir " ->
        silver' cwd (insert (Cwd.pwd cwd) (read_dir cwd dir) dt) xs
    | file :: xs when Str.string_match (Str.regexp "[0-9].*") file 0 ->
        silver' cwd (insert (Cwd.pwd cwd) (read_file file) dt) xs
    | [] -> dt
    | _ -> assert false
  in
  silver' cwd (Dir ("/", [], 0)) parsed |> init_immediate_dirsizes

let sum_smallish_dirs =
  fold
    (fun acc dt ->
      match dt with
      | File _ -> acc
      | Dir (_, _, size) when size <= 100000 -> acc + size
      | Dir _ -> acc)
    0

let silver () = directory_tree |> sum_smallish_dirs

module Closer = struct
  type t = { mutable current : int; target : int }

  let create target = { current = Int.max_num; target }
  let get closer = closer.current

  let push closer n =
    if n >= closer.target && n < closer.current then closer.current <- n
end

let gold () =
  let used_space = function Dir ("/", _, size) -> size | _ -> assert false in
  let total_disk_size = 70000000 in
  let free_space = total_disk_size - used_space directory_tree in
  let needed_free_space = 30000000 in
  let needs_to_reclaim = needed_free_space - free_space in
  let answer = Closer.create needs_to_reclaim in
  let _ =
    map
      (function
        | File _ as f -> f
        | Dir (_, _, size) as dir ->
            Closer.push answer size;
            dir)
      directory_tree
  in
  Closer.get answer

let _ = Aoc.print_results (silver ()) (gold ())
