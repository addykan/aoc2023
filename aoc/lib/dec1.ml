let read_lines filename =
  let channel = open_in filename in
  let rec read_lines_aux acc =
    try
      let line = input_line channel in
      read_lines_aux (line :: acc)
    with End_of_file -> acc
  in
  let lines = read_lines_aux [] in
  close_in channel;
  List.rev lines  (* Reverse the list to maintain the original order *)

(* https://discuss.ocaml.org/t/should-we-have-a-string-rev-in-stdlib/9187/3 *)
let rev x =
  let len = String.length x in
  String.init len (fun n -> String.get x (len - n - 1))

let rec getFirstInt (s: string) : int =
  match s with 
  | "" -> 0
  | _ -> ( match int_of_string_opt (String.sub s 0 1) with 
    | Some x -> x
    | None -> getFirstInt (String.sub s 1 (String.length s - 1)) )

let getLastInt (s: string) : int = 
  getFirstInt (rev s)

let result : (int) = 
  let filename = "/Users/addykan/Github/aoc2023/aoc/lib/inputs/1.txt" in
  let string_list = read_lines filename in
  let firstIntList = List.map getFirstInt string_list in
  let lastIntList = List.map getLastInt string_list in
  let zipped = List.combine firstIntList lastIntList in
  let makeNum (x, y) = x * 10 + y in
  let numList = List.map makeNum zipped in
  List.fold_left (fun acc x -> acc + x) 0 numList
  

