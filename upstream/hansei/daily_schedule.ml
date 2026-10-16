(*
		Logic programming in Hansei

Solving the logical puzzle (scheduling problem) posed by Mikael More
on October 25, 2010:

 - For the daily schedule for Monday to Wednesday:

   One of the days I'll shop.
   One of the days I'll take a walk.
   One of the days I'll go to the barber.
   One of the days I'll go to the supermarket.
   The same day as I go to the supermarket, I'll shop.
   The same day as I take a walk I'll go to the barber.
   I'll go to the supermarket the day before the day I'll take a walk.
   I'll take a walk Tuesday.

   Which are all possible daily schedules, regarding those four events?

*)

(* We take the phrase `One of the days I'll shop.' to mean that
   `I shop only on one day out of the three.'
   Otherwise, there are too many solutions: for example, I can do
   all the activities on each day.
*)

(* If using the OCaml top-level, execute the following directives:
#load "prob.cma";;
*)
open ProbM;;


(* sample from a powerset *)
let powerset lst =
  List.fold_left (fun acc e -> if flip 0.5 then e::acc else acc) [] lst;;

(* Check the sampling procedure: obtain the probability table *)
let [(0.125, Ptypes.V [3; 2; 1]); 
     (0.125, Ptypes.V [3; 2]);
     (0.125, Ptypes.V [3; 1]); 
     (0.125, Ptypes.V [3]); 
     (0.125, Ptypes.V [2; 1]);
     (0.125, Ptypes.V [2]);
     (0.125, Ptypes.V [1]);
     (0.125, Ptypes.V [])]
    =
exact_reify (fun () -> powerset [1;2;3]);;

type action = Shop | Walk | Barber | Supermarket;;

(* Assert that the proposition x must hold in the current world *)
let mustbe x = if not x then fail ();;

(* Describe the problem, declaratively *)
let schedule_model () =
  let ndays = 3 in
  (* A set of actions I can do on day n *)
  let actions = memo (fun n -> 
                      powerset  [Shop; Walk; Barber; Supermarket]) in
  
  (* [action_on a d] is a proposition stating that on day [d]
     I do action [a]
   *)
  let action_on a d = (List.mem a (actions d)) in

  (* [only_on a d] is a proposition stating that I do action [a]
     _only_ on day [d] (and not on any other day)
   *)
  let only_on a d = [d] = List.filter (action_on a) [0;1;2] in

  (* State all the constraints of the problem *)
  let d = uniform ndays in
  let _ = mustbe (only_on Shop d) in
  let d = uniform ndays in
  let _ = mustbe (only_on Walk d) in
  let d = uniform ndays in
  let _ = mustbe (only_on Barber d) in
  let d = uniform ndays in
  let _ = mustbe (only_on  Supermarket d) in

  (* The same day as I go to the supermarket, I'll shop.
     That is, there is a day that I perform the action Shop and
     the action Supermarket
  *)
  let d = uniform ndays in
  let _ = mustbe (action_on Supermarket d && action_on Shop d) in

  let d = uniform ndays in
  let _ = mustbe (action_on Walk d && action_on Barber d) in

  (* I'll go to the supermarket the day before the day I'll take a walk. *)
  let d = uniform ndays in
  let _ = mustbe (action_on Walk d &&
		  d > 0 && action_on Supermarket (d-1)) in

  (* I'll take a walk Tuesday. *)
  let _ = mustbe (action_on Walk 1) in

  (* Return the schedule: the array of actions for each day *)
  Array.init ndays actions 
;;

(* val schedule_model : unit -> action list array = <fun> *)

let [(1.11632658893461385e-07,
  Ptypes.V [|[Supermarket; Shop]; [Barber; Walk]; []|])] =
exact_reify schedule_model;;

(*
- : action list array Ptypes.pV =
[(1.11632658893461385e-07,
  Ptypes.V [|[Supermarket; Shop]; [Barber; Walk]; []|])]

That is, on Monday I go to the supermarket and shop, on Tuesday I walk
and take a haircut. There is only one schedule satisfying the constraints.
1.11e-07 is the estimate of the search space.
*)

print_endline "\nAll done";;
