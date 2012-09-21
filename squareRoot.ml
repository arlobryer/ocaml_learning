(* Program to evaluate a sqrt using Newton's method *)

(* Abs value function - it'll be in a library but here is an implementation
for practice *)
let absVal x =
  if x < 0.
  then 0. -. x
  else x;;

(* (\* This is the implementation of Newton's method *\) *)
let improve best x =
  (best +. x /. best) /. 2.;;

(* (\* This decides when we're close enough *\) *)
let goodEnough best x =
  if absVal (best *. best /. x -. 1.) < 0.001
  then true
  else false;;

(* (\* This is the recursive function to call the above two *\) *)
let rec sqrtIter best x =
  if goodEnough best x
  then best
  else sqrtIter(improve best x) x;;


(* Finally the sqrt function *)
let sqrt y =
  sqrtIter 1.0 y;;

sqrt 2.;;
 
