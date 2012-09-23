(* Some examples of recursive functions taken from 
the coursera scala / intro to func. programming course *)

(* evaluate element c, r in Pascal's triangle *)

let rec pascal c r =
  if (c == r || c == 0) then 1
  else pascal c (r - 1) + pascal (c - 1) (r - 1);;


let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

let rec print_list = function 
[] -> ()
| e::l -> print_char e ; print_string " " ; print_list l;;

(* check for parens matching *)
let matcher s = 
  let chars = explode s in 
  print_list(chars);
  let rec rec_match (count : int) (chars : char list) : int =
    if count < 0 then -1
    else if List.length chars == 0 then count
    else if List.hd chars == '(' then rec_match (count + 1) (List.tl chars)
    else if List.hd chars == ')' then rec_match (count - 1) (List.tl chars)
    else rec_match count (List.tl chars) in
  if List.length chars == 0 then true
  else rec_match 0 chars == 0;;

matcher "this() is( an expression)";;

let rec coin_denomination amount coins =
  if amount = 0 then 1
  else if (amount < 0 || List.length coins == 0) then 0
  else coin_denomination (amount - List.hd coins) coins + 
    coin_denomination amount (List.tl coins);;

coin_denomination 100 [1; 2; 5; 10];;

    
