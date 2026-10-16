(* Solving the Zebra puzzle with Hansei *)

(* We use lazy permutations to implement sets of logic variables upon
   the ``all different'' constraint, which appears prominently
   in the statement of the Zebra puzzle:
*)

(*
 1. There are five houses in a row, each of a different color
     and inhabited by men of different nationalities,
     with different pets, drinks, and cigarettes.
 2. The Englishman lives in the red house.
 3. The Spaniard owns a dog.
 4. Coffee is drunk in the green house.
 5. The Ukrainian drinks tea.
 6. The green house is directly to the right of the ivory house.
 7. The Old Gold smoker owns snails.
 8. Kools are being smoked in the yellow house.
 9. Milk is drunk in the middle house.
10. The Norwegian lives in the first house on the left.
11. The Chesterfield smoker lives next to the fox owner.
12. Kools are smoked in the house next to the house where the horse is kept.
13. The Lucky Strike smoker drinks orange juice.
14. The Japanese smokes Parliaments.
15. The Norwegian lives next to the blue house.
*)

(* If using the OCaml top-level, execute the following directives:
#load "prob.cma";;
#load "lazy_permutations.cmo";;
*)
open ProbM;;
open Lazy_permutations;;

(* First, we define the domains *)
(* We could have used polymorphic variants and avoided all the declarations
   below.
   But declarations give better error messages and prevent stupid
   errors.
*)
type color       = Red | Green | Ivory | Yellow | Blue;;
type nationality = EN | SP | UA | NO | JA;;
type pet         = Dog | Snail | Fox | Horse | Zebra;;
type drink       = Coffee | Tea | Milk | OJ | Water;;
type smoke       = OGold | Kools | Chest | LStrike | Parliaments;;


(* Assert that the proposition x must hold in the current world *)
let mustbe x = if not x then fail ();;

(* Assert that the predicate f holds on some element of the list *)
let rec exists f = function
   | []     -> fail ()
   | (h::t) -> if f h then () else exists f t
;;
(*  val exists : ('a -> bool) -> 'a list -> unit = <fun> *)
 
(* Build a list [0..N-1] *)
let iota n =
   let rec loop i =
   if i >= n then [] else i::loop (i+1)
   in loop 0
;;
let [0; 1; 2; 3; 4] = iota 5;;		(* test case, or example *)

(* Given the number of houses `nhouses' and the house number 'i'
   compute the number of the house that is directly to the right of 'i'
*)
let to_the_right nhouses i =
  let j = i + 1 in
  if j >= nhouses then fail () else j
;;

(* Given the number of houses `nhouses' and the house number 'i'
   compute the number of the house that is directly to the left of 'i'
*)
let to_the_left nhouses i =
  let j = i - 1 in
  if j < 0 then fail () else j
;;

(* Given the number of houses `nhouses', non-deterministically
   pick a function to compute the house next to the given one.
*)
let next_to_dir nhouses =
  if flip 0.5 then to_the_left nhouses else to_the_right nhouses
;;


(* Make a selector that gives the h-th element from a random
   permutation of arr
*)
let make_selector arr =
  let perm = Array.of_list (lazy_permutation (Array.to_list arr)) in
  fun h -> perm.(h) ()
;;

(* A different version, less efficient, as it turns out *)
(*
let make_selector arr =
  let perm = Array.of_list (lazy_permutation_ins (Array.length arr)) in
  fun h -> arr.(perm.(h) ())
*)


(* We define the zebra model by directly translating the English
   description of the problem.
*)
let zebra_model () =
  let nhouses = 5 in
  let houses = iota nhouses in

  (* we define functions that map the house number to its color,
     the nationality of the inhabitant, etc.
  *)
  let color = make_selector [|Red; Green;Ivory;Yellow;Blue|] in
  let nat =   make_selector [|EN; SP; UA; NO; JA|] in
  let pet =   make_selector [|Dog; Snail; Fox; Horse; Zebra|] in
  let drink = make_selector [|Coffee; Tea; Milk; OJ; Water|] in
  let smoke = make_selector [|OGold; Kools; Chest; LStrike; Parliaments|] in

  (* We encode the known facts. The code is essentially deterministic *)

  (* The Norwegian lives in the first house on the left. *)
  mustbe (nat 0 = NO);

  (* Milk is drunk in the middle house. *)
  mustbe (drink 2 = Milk);

  (* The Englishman lives in the red house.
     Or: there is a house whose color is red and which is inhabited
     by an Englishman.
  *)
  exists (fun h -> nat h = EN && color h = Red) houses;

  (* The Spaniard owns a dog. *)
  exists (fun h -> nat h = SP && pet h = Dog) houses;

  (* Coffee is drunk in the green house. *)
  exists (fun h -> drink h = Coffee && color h = Green) houses;

  exists (fun h -> nat h = UA && drink h = Tea) houses;

  exists (fun hl ->
   let hr = to_the_right nhouses hl in
   color hr = Green && color hl = Ivory) houses;

  exists (fun h -> smoke h = OGold && pet h = Snail) houses;

  exists (fun h -> smoke h = Kools && color h = Yellow) houses;

  (* The Chesterfield smoker lives next to the fox owner. *)
  exists (let next_of = next_to_dir nhouses in
          fun h -> smoke h = Chest && pet (next_of h) = Fox) houses;

  exists (let next_of = next_to_dir nhouses in
          fun h -> smoke h = Kools && pet (next_of h) = Horse) houses;

  exists (fun h -> smoke h = LStrike && drink h = OJ) houses;

  exists (fun h -> nat h = JA && smoke h = Parliaments) houses;

  exists (let next_of = next_to_dir nhouses in
          fun h -> nat h = NO && color (next_of h) = Blue) houses;

  (* The model returns the attributes of all the houses, in a list. *)
  List.map (fun h -> (color h, nat h, pet h, drink h, smoke h)) houses
;;

(* 
val zebra_model : unit -> (color * nationality * pet * drink * smoke) list
*)



(* The results show that the solution is unique.
   The probability of evidence, 5e-12, tells the size of the
   search space.
*)

let (5.023469650205764e-12,
 [(1.,
   Ptypes.V
    [(Yellow, NO, Fox,   Water,  Kools); 
      (Blue,   UA, Horse, Tea,    Chest);
      (Red,    EN, Snail, Milk,   OGold); 
      (Ivory,  SP, Dog,   OJ,     LStrike);
      (Green,  JA, Zebra, Coffee, Parliaments)])])
    =
Inference.normalize (exact_reify zebra_model);;

let _ = print_endline "All done";;
