
(* #require "str";; *)
(* Sys.chdir("../");; *)
(* Sys.getcwd();; *)



type op =
  | TurnOn
  | TurnOff
  | Toggle

type instruction = {
  op : op;
  x1 : int;
  y1 : int;
  x2 : int;
  y2 : int;
}

let parse_line s =
  let re =
    Str.regexp
      "\\(turn on\\|turn off\\|toggle\\) \
       \\([0-9]+\\),\\([0-9]+\\) \
       through \
       \\([0-9]+\\),\\([0-9]+\\)"
  in
  if Str.string_match re s 0 then
    let op =
      match Str.matched_group 1 s with
      | "turn on" -> TurnOn
      | "turn off" -> TurnOff
      | "toggle" -> Toggle
      | _ -> failwith "impossible"
    in
    {
      op;
      x1 = int_of_string (Str.matched_group 2 s);
      y1 = int_of_string (Str.matched_group 3 s);
      x2 = int_of_string (Str.matched_group 4 s);
      y2 = int_of_string (Str.matched_group 5 s);
    }
  else
    failwith ("bad line: " ^ s)
    

let read_lines filename =
  In_channel.with_open_text filename (fun ic ->
    In_channel.input_lines ic
    |> List.filter (fun s -> String.trim s <> "")
  )

let lines = read_lines "../../input.txt"

let parsed_lines = List.map parse_line lines

(* make grid bigger so we can just do 1 ... 1000 easier for me - ignore row col = 0 or 1001  *)
let grid = Array.make_matrix 1002 1002 false 

let grid_reset () =
  for x = 0 to 1001 do
    for y = 0 to 1001 do
      grid.(x).(y) <- false
    done
  done

let grid_count () =
  let count = ref 0
  in 
  for x = 1 to 1000 do
    for y = 1 to 1000 do
      if grid.(x).(y) = true then incr count
      else ()
    done
  done ;
  !count


let toggle x1 y1 x2 y2 =
  for x = x1 to x2 do
    for y = y1 to y2 do
      grid.(x).(y) <- not grid.(x).(y)
    done 
  done 

let turn_on x1 y1 x2 y2 =
  for x = x1 to x2 do
    for y = y1 to y2 do
      grid.(x).(y) <- true
    done 
  done 

let turn_off x1 y1 x2 y2 =
  for x = x1 to x2 do
    for y = y1 to y2 do
      grid.(x).(y) <- false
    done 
  done 


let faction instr =
  match instr with
| { op = Toggle; x1; y1; x2; y2 } -> toggle x1 y1 x2 y2
| { op = TurnOn; x1; y1; x2; y2 } -> turn_on x1 y1 x2 y2
| { op = TurnOff; x1; y1; x2; y2 } -> turn_off x1 y1 x2 y2


let part1 () =
  grid_reset () ;
  List.iter faction parsed_lines ;
  grid_count () 
  
(* lets compute ! *)
(* let _ = Printf.printf "part 1 has %d nice strings " (List.length part1) *)

let grid2 = Array.make_matrix 1002 1002 0

let grid_reset2 () =
  for x = 0 to 1001 do
    for y = 0 to 1001 do
      grid2.(x).(y) <- 0
    done
  done

let grid_count2 () =
  let count = ref 0
  in 
  for x = 1 to 1000 do
    for y = 1 to 1000 do
      count := (!count) + grid2.(x).(y)
    done
  done ;
  !count



let toggle2 x1 y1 x2 y2 =
  for x = x1 to x2 do
    for y = y1 to y2 do
      grid2.(x).(y) <- grid2.(x).(y) + 2 
    done 
  done 


let turn_on2 x1 y1 x2 y2 =
  for x = x1 to x2 do
    for y = y1 to y2 do
      grid2.(x).(y) <- grid2.(x).(y) + 1 
    done 
  done 


(* limited to bottom of zero *)
let turn_off2 x1 y1 x2 y2 =
  for x = x1 to x2 do
    for y = y1 to y2 do
      grid2.(x).(y) <- max (grid2.(x).(y) - 1) 0
    done 
  done 


let faction2 instr =
  match instr with
| { op = Toggle; x1; y1; x2; y2 } -> toggle2 x1 y1 x2 y2
| { op = TurnOn; x1; y1; x2; y2 } -> turn_on2 x1 y1 x2 y2
| { op = TurnOff; x1; y1; x2; y2 } -> turn_off2 x1 y1 x2 y2


let part2 () =
  grid_reset2 () ;
  List.iter faction2 parsed_lines ;
  grid_count2 () 


let _ = let p = part1 () in Printf.printf "part1 solution %d\n" p
let _ = let p = part2 () in Printf.printf "part2 solution %d\n" p
