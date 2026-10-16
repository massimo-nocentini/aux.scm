(*
   Airplane-blip example from the BLOG papers of Brian Milch et al.
   This example was mentioned in Stuart Russell's talk at the
   NIPS2008 Probabilistic Programming workshop. 
   This example is meant to show-case open worlds and BLOG's ability
   to reason with open worlds.

The blip example is HMM with a twist: the number of airplanes is not
fixed; rather, it is described by a geometric distribution. Any number
of airplanes is possible (although with progreessively decreasing
probabilities).

The current code is based on the AILog implementation of the blip example,
available on the AILog2 web page.

To run the example:
pl -s ../ailog/ailog2.pl
ailog: load 'blip.cil'.

ailog: observe blip(3,3,0) & blip(3,4,next(0)).
Answer: P(blip(3, 3, 0) & blip(3, 4, next(0))|Obs)=[0.000544527, 0.00571287].
 Runtime since last report: 0.0 secs.
  [ok,more,explanations,worlds,help]: worlds.
  0: [~blipRandomlyOccurs(3, 3, 0), ~more(aircraft, 1, 0.15), producesBlip(1, 0), producesBlip(1, next(0)), direction(1, 0, north), more(aircraft, 0, 0.15), xpos(1, 0, 3), ypos(1, 0, 3), xDer(1, 0, north, 0), yDer(1, 0, north, 1)]  Prob=0.000141693
  1: [~more(aircraft, 1, 0.15), blipRandomlyOccurs(3, 3, 0), blipRandomlyOccurs(3, 4, next(0))]  Prob=0.00034
  2: [blipRandomlyOccurs(3, 3, 0), blipRandomlyOccurs(3, 4, next(0)), more(aircraft, 1, 0.15)]  Prob=6e-05
  3: [~blipRandomlyOccurs(3, 4, next(0)), ~more(aircraft, 1, 0.15), producesBlip(1, 0), producesBlip(1, next(0)), blipRandomlyOccurs(3, 3, 0), direction(1, 0, north), more(aircraft, 0, 0.15), xpos(1, 0, 3), ypos(1, 0, 3), xDer(1, 0, north, 0), yDer(1, 0, north, 1)]  Prob=2.83387e-06
Answer: P(blip(3, 3, 0) & blip(3, 4, next(0))|Obs)=[0.000544527, 0.00571287].
 Runtime since last report: 0.0 secs.
  [ok,more,explanations,worlds,help]: ok.

ailog: load 'blip.cil'.
ailog: observe blip(3,3,0) & blip(3,4,next(0)) & blip(7,8,next(0)) & blip(3,5,next(next(0))).
No (more) answers.
[This means that AILog is unable to compute the answer given its self-imposed
restrictions on the search space. I think AILog probably restricts the
search depth.]

ailog: load 'blip.cil'.
ailog: observe blip(3,3,0) & blip(7,8,next(0)) & blip(3,5,next(next(0))).
No (more) answers.

ailog: observe blip(7,8,next(0)) & blip(3,5,next(next(0))).
Answer: P(blip(7, 8, next(0)) & blip(3, 5, next(next(0)))|Obs)=[0.0004, 0.0334648].
 Runtime since last report: 0.03125 secs.

*)


open Ptypes
open ProbM

type dir = North | East | South | West;;


(* ---------------  Priors *)

let number_aircrafts () = geometric 0.85;;

let npos = 10;;		  (* all coordinates are within 0..npos-1 *)
(* Initial (x,y) positions of the plane i *)
let xpos0 i = uniformly [|0;1;2;3;4;5;6;7;8;9|];;
let ypos0 i = uniformly [|0;1;2;3;4;5;6;7;8;9|];;

(* Initial direction of the plane i *)
let dir0 i = uniformly [|North; East; South; West|];;

(* x- or y-Derivatives for the plane i at time t travelling in the direction
   dir
*)
let xderiv i t = function
  | North -> dist [(0.1, -1); (0.8,  0); (0.1, 1)]
  | East  -> dist [(0.2,  0); (0.7,  1); (0.1, 2)]
  | South -> dist [(0.1, -1); (0.8,  0); (0.1, 1)]
  | West  -> dist [(0.2,  0); (0.7, -1); (0.1, 2)]
;;

let yderiv i t = function
  | North -> dist [(0.2,  0); (0.7,  1); (0.1, 2)]
  | East  -> dist [(0.1, -1); (0.8,  0); (0.1, 1)]
  | South -> dist [(0.2,  0); (0.7, -1); (0.1, 2)]
  | West  -> dist [(0.1, -1); (0.8,  0); (0.1, 1)]
;;

(* The new direction for the plane i travelling in the direction dir
   at the time t
*)
let new_dir i t = function
  | North -> dist [(0.2, West);  (0.6, North); (0.2, East)]
  | East  -> dist [(0.2, North); (0.6, East);  (0.2, South)]
  | South -> dist [(0.2, East);  (0.6, South); (0.2, West)]
  | West  -> dist [(0.2, South); (0.6, West);  (0.2, North)]
;;

type pos = int * int;;

(* ---------------  The equations of motion and radar detection *)

(* In this first attempt, we have to use memoization. Indeed,
   if we know that plane 1 was observed at certain position at time t,
   that fact remains no matter how many times we refer to it.

   Currently, the expression involing memo has to be executed only when
   inference is in progress. That is, we can't define memoized top-level
   functions just like

     let xpos0 = memo ( ... )

   Generally speaking, this restriction is artificial and can be lifted.
   After all, executing memo per se does not involve any delimited control.
   For now, to avoid thunking of all functions, we use a functor.

*)


(* Memoizing fixpoint combinator *)
let fixm f = 
   let self = ref (fun v -> failwith "blackhole") in
   let g = memo (fun v -> f !self v) in
   self := g; g
;;

let rec one_of f = function 0 -> false
  | n -> f n || one_of f (pred n)
;;

module BLIPMODEL(S:sig end) = struct
  let xpos0 = memo xpos0		(* memoize the priors *)
  let ypos0 = memo ypos0
  let dir0  = memo dir0
  let new_dir = memo (fun (i,t,dir) -> new_dir i t dir)
  let xderiv  = memo (fun (i,t,dir) -> xderiv i t dir)
  let yderiv  = memo (fun (i,t,dir) -> yderiv i t dir)

  (* Direction of the plane i at time t *)
  let direction =
    let loop self = function (i,0) -> dir0 i
      | (i,t) -> new_dir (i, pred t, self (i,pred t))
    in fixm loop

  (* Position of the plane i at time t *)
  let pos : int * int -> pos =
    let loop self = function 
      | (i,0) -> (xpos0 i, ypos0 i)
      | (i,t1) -> 
	  let t = pred t1 in
	  let (x,y)  = self (i,t) in
	  let dir    = direction (i,t) in
	  let x'     = xderiv (i,t,dir) in
	  let y'     = yderiv (i,t,dir) in
	  let newx   = x + x' in
	  let ()     = if newx < 0 || newx > 9 then fail () in
	  let newy   = y + y' in
	  let ()     = if newy < 0 || newy > 9 then fail () in
	  (newx,newy)
    in fixm loop


  (* The radar model from the AILog example code. It is like noisy OR *)

  (* anyblip pos t is true if there is a blip at the position p at time t *)

  let blip_from i p t =			(* blip from plane #i *)
    let p' = pos (i,t) in
    p = p' && flip 0.9			(* Don't memoize the flip *)

  let anyblip1 = memo(
    fun (p,t) ->
      flip 0.02 ||				(* randomly occurring blip *)
      let nplanes = 1 in
      one_of (fun i -> blip_from i p t) nplanes)

  let anyblip =
    let nplanes = letlazy number_aircrafts in
    memo (fun (p,t) ->
      flip 0.02 ||				(* randomly occurring blip *)
      one_of (fun i -> blip_from i p t) (nplanes ()))
end;;


(* tests *)

(* First check the memoization *)
let [(0.0100000000000000019, V ())] =
  exact_reify (fun () -> 
    let module M = BLIPMODEL(struct end) in
    if M.pos (1,0) = (3,3) && M.pos (1,0) = (3,3) 
    then () else fail ());;

let [] = 
  exact_reify (fun () -> 
    let module M = BLIPMODEL(struct end) in
    if M.pos (1,0) = (3,3) && M.pos (1,0) = (3,4) 
    then () else fail ());;

let [(0.0015, V ())] =
  exact_reify (fun () -> 
    let module M = BLIPMODEL(struct end) in
    if M.pos (1,0) = (3,3) && M.pos (1,1) = (3,4) 
    then () else fail ());;

let [(0.0288200000000000019, V true); (0.971180000000001153, V false)] =
  exact_reify (fun () -> 
    let module M = BLIPMODEL(struct end) in
    M.anyblip1 ((3,3),0));;

(* Again check memoization: facts observed once stay the same *)
let [(0.0288200000000000019, V true); (0.971180000000001153, V false)] =
  exact_reify (fun () -> 
    let module M = BLIPMODEL(struct end) in
    M.anyblip1 ((3,3),0) && M.anyblip1 ((3,3),0));;

let [(0.00191968600000000031, V true); (0.995959594000000892, V false)] =
  exact_reify (fun () -> 
    let module M = BLIPMODEL(struct end) in
    M.anyblip1 ((3,3),0) && M.anyblip1 ((3,4),1));;
    
(* if we multiply by 0.15 *. 0.85 (probability of exactly 1 aircraft)
   and by 0.81 (prob of aircraft being detected at times 0 and 1)
  we get 0.000198255571650000033, which is higher than
  computed by AILog because we take into account more possibilities:
  the y coordinate may increase also because the plane flies to the East
  and West. The worlds presented by AILog do not show these possibilities *)

(* % observe blip(3,3,0) & blip(3,4,next(0)). *)

let query1 () = 
  let module M = BLIPMODEL(struct end) in
  M.anyblip ((3,3),0) && M.anyblip ((3,4),1);;

(* The probability is higher than that computed by AILog: we consider
   more possibilities.
*)
let [(0.00082706367283999938, V true); (0.998667405620373705, V false)] =
  sample_importance (random_selector 17) 1000 query1;;

let [(0.000636272786400000141, V true); (0.998496271001613422, V false)] =
  sample_importance (random_selector 19) 1000 query1;;

