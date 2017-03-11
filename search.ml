open Ast
open Printf
open List
open PrioQueue

let graph = Hashtbl.create 10000

let string_of_coord x y = "x" ^ (x |> string_of_int)  ^ "y" ^ (y |> string_of_int)

let coord_of_string str =
 (* get rid of leading 'x' *)
 let tail   = String.sub str 1 ((String.length str) - 1) in
 let coords = String.split_on_char 'y' tail in
 (* list should have length 2 - x coord and y coord *)
 (match coords with
   | x::xs  -> ((x |> int_of_string),((List.hd xs) |> int_of_string))
   | _     -> failwith "coord_of_string")

let rec remove ls fromls = filter (fun x -> not(mem x ls)) fromls

let rec clean = function
 | [] -> []
 | (None)::xs   -> clean xs
 | (Some x)::xs -> x :: clean xs


let rec expand vertex visited = let {name; xLoc; yLoc; status; successors;parent} = Hashtbl.find graph vertex in
       let new_successors = (match status with
           | Free                 -> clean [(List.nth successors 2);(List.nth successors 0);(List.nth successors 4); (List.nth successors 6)] |> remove visited
           | OccupiedHorizontal   -> clean [(List.nth successors 0);(List.nth successors 4)] (* can only go up/down *)    |> remove visited
           | OccupiedVertical     -> clean [(List.nth successors 2);(List.nth successors 5)] (* can only go left/right *) |> remove visited
           | Blocked              -> [] ) in
         for i = 0 to (List.length new_successors -1) do
           let {name; xLoc; yLoc; status; successors;  parent} = Hashtbl.find graph (List.nth new_successors i) in
           Hashtbl.replace graph (List.nth new_successors i) {name = name;
                                                              xLoc = xLoc;
                                                              yLoc = yLoc;
                                                              status = status;
                                                              successors = successors;
                                                              parent = vertex}
         done;
         remove visited new_successors

let rec strategy' oldf newf visited goal cost_so_far = match newf with
 | []        -> oldf
 | x::xs     -> let (node_x,node_y) = coord_of_string x in
                let (goal_x,goal_y) = coord_of_string goal in
                let manhattan_dist = abs(node_x - goal_x) + abs(node_y - goal_y) * 10 in
               (*  let {name; xLoc; yLoc; status; successors; parent} = Hashtbl.find graph x in *)
                strategy' (PrioQueue.insert oldf (manhattan_dist + cost_so_far) x visited) xs visited goal cost_so_far

let rec remove_duplicates = function
 | []                  -> []
 | [x]                 -> [x]
 | (x,y)::(x',y')::xs  -> if (x = x' && y = y')
                          then remove_duplicates ((x',y')::xs)
                          else (x,y) :: (remove_duplicates ((x',y')::xs))

let rec find elt ls = match ls with
 | []        -> failwith "could not find"
 | (o,i)::xs -> if o = elt then i else find elt xs

let rec reconstruct_path elt goal =
         if elt = goal then
           [goal]
         else
           let ({name;xLoc;yLoc;status;successors;parent}) = Hashtbl.find graph elt in
           name :: reconstruct_path parent goal

let rec search start goal fringe visited cost = match fringe with
 | PrioQueue.Empty    -> failwith "No route exists"
 | PrioQueue.Node(prio,x,left,right) ->
   let ({name;xLoc;yLoc;status;successors;parent}) = Hashtbl.find graph x in
   if goal = name then
      reconstruct_path name start
   else
     (search start goal (strategy' (PrioQueue.remove_top (PrioQueue.Node(prio,x,left,right))) (expand x (visited)) (x::visited) goal cost) (x::visited) (cost+3))
