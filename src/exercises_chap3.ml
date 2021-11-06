(* return the product of a list *)
let rec product = function
  | [] -> 1
  | h :: t -> h * product t

let rec concat_str_list = function
  | [] -> ""
  | h :: tail -> h ^ (concat_str_list tail)

(* not exhaustive *)
let fst_elem1 (h :: _) = h = "bigred"

let two_or_four lst =
  let rec len = function
    | [] -> 0
    | _ :: t -> 1 + len t
  in
  let lst_len = len lst in
  if lst_len = 2 || lst_len = 4 then true else false
(* or way easier *)
let two_or_four = function
  | [_; _] -> true
  | [_; _; _; _] -> true
  | _ -> false

(* not exhaustive *)
let fst_scnd_equal (f :: s :: _) = f = s

let fifth = function
  | [] -> 0
  | lst -> if (List.length lst) >= 5 then List.nth lst 5 else 0 

let last_elem lst =
  (List.length lst) - 1
  |> List.nth lst

let any_zeroes lst = List.exists (fun x -> x = 0) lst

(* exercise: take drop *)
(**let rec take n lst =
  match (n, lst) with
  | (0, l) -> l
  | (_, []) -> lst
  | (n, h :: t) -> h :: (take' (n-1) t) *)

let rec take n lst =
  match lst with
  | [] -> []
  | _ :: _ when n = 0 -> []
  | h :: t when n > 0 -> h :: (take (n-1) t)

let rec drop n lst =
  match lst with
  | [] -> []
  | _ :: t when n > 0 -> drop (n-1) t
  | h :: t when n = 0 -> h :: (drop 0 t)

(* TODO: tail recursion *)
(* let rec take_tr n lst ?acc:(accu=[]) *)

(* unimodal *)
(* surely not the most smart or efficient way, but it works *)
let is_unimodal lst =
  let betrag x =
    if x < 0 then x * (-1) else x
  in 
  let rec unimod lst prev const =
    match lst with
    | [] -> true
    | h :: t -> if betrag (h - prev) <> const
                then false else unimod t h const
  in
  let c = (List.nth lst 1) - (List.nth lst 0)
  in unimod lst 0 c

(* print int list *)
let rec print_int_list = function
  | [] -> ()
  | h :: t -> string_of_int h
              |> print_endline;
              print_int_list t

let print_int_list_iter lst =
  List.iter (fun x -> string_of_int x |> print_endline) lst

(* exercise: student *)
type student = { first_name : string; last_name : string; gpa : float }
let jolinde = { first_name = "Jolinde"; last_name = "Frosch"; gpa = 100.0 }
let full_name { first_name; last_name } = (first_name, last_name)
let cons_student first_name last_name gpa = { first_name; last_name; gpa }

(* pokerecord *)
type poketype = Normal | Fire | Water
type pokemon = { name : string; hp : int; ptype : poketype}
let charizard = { name = "Charizard"; hp = 78; ptype = Fire }
let squirtle = { name = "Squirtle"; hp = 44; ptype = Water }

(* safe hd and tl *)
let safe_hd = function
  | [] -> None
  | h :: _ -> Some h

let safe_tl = function
  | [] -> None
  | _ :: t -> Some t

(* pokefun *)
let head (h :: _) = h
let max_hp = function
  | [] -> None
  | lst -> let r = List.sort (fun p1 p2 -> p2.hp - p1.hp) lst
                   |> head
           in Some r

(* date before && earliest date *)

(* checks wether date1 is before date2 *)
let is_before date1 date2 =
  let (y1, m1, d1) = date1 in
  let (y2, m2, d2) = date2 in
  let res = (y2 - y1, m2 - m1, d2 - d1) in
  let prod (x, y, z) = x * y * z in
  if (prod res) < 0 then false else true

(* returns the earliest date in a list of dates *)
let earliest = function
  | [] -> None
  | lst -> let res = List.sort (fun d1 d2 -> is_before d1 d2) lst
           in Some (List.nth 0 lst)
