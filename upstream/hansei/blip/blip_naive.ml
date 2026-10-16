(*
   Airplane-blip example from the BLOG papers of Brian Milch et al.
   This example was mentioned in Stuart Russell's talk at the
   NIPS2008 Probabilistic Programming workshop.
   See also example 1.3 of
   http://people.csail.mit.edu/milch/papers/blog-chapter.pdf

   This example is meant to show-case open worlds and BLOG's ability
   to reason with open worlds.

The blip example is HMM with a twist: the number of airplanes is not
fixed; rather, it is described by a geometric distribution. Any number
of airplanes is possible (although with progressively decreasing
probabilities).

We write our model explicitly as a HMM with a variable number of objects, 
evolving stochastically.
We assert things like blip() as the evidence. So, we can answer 
AILog queries like
     observe blip(3,3,0) & blip(3,4,next(0))
as computing the probability of evidence on these two blips. But we
can answer more queries: the distribution on the number of airplanes
that are consistent with the observation; prediction on the trajectories
of selected planes; probability that two blips are for the same plane, etc.

In this file, we do not use laziness. This works for only one, max two
planes. We also interpret the evidence about blips as partial: if we
are told of the observed blip at position (3,3) we assume nothing is
known about other positions on the radar screen. There may be blips at
the other positions, too -- but we are not told about them.

*)


open Ptypes
open ProbM

type dir = North | East | South | West;;


(* ---------------  Priors *)
(* We use the parameters from the AILog2 implementation of the example.
   See also blip_ailog.ml.
*)

let number_aircrafts () = geometric 0.85;;

let npos = 10;;		  (* all coordinates are within 0..npos-1 *)
(* Initial (x,y) positions of the plane i *)
let xpos0 i = uniform npos;;
let ypos0 i = uniform npos;;

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

(* ---------------  The equations of motion and radar detection. *)


(* The state of each plane at a given time moment *)
type plane_state = {plane_idx : int;
                    plane_pos : pos;
                    plane_dir : dir};;

(* obtain a sample initial plane state, for a plane number i *)
let plane_state_0 i =
   {plane_idx = i; plane_pos = (xpos0 i, ypos0 i); plane_dir = dir0 i};;

(* Evolve the state of one plane to the next time moment *)
let plane_fly t pstate : plane_state =
   let i = pstate.plane_idx in
   let x' = xderiv i t pstate.plane_dir in
   let y' = yderiv i t pstate.plane_dir in
   let (x,y) = pstate.plane_pos in
   let newx   = x + x' in
   let ()     = if newx < 0 || newx > 9 then fail () in
   let newy   = y + y' in
   let ()     = if newy < 0 || newy > 9 then fail () in
   {plane_idx = i; plane_pos = (newx,newy); 
    plane_dir = new_dir i t pstate.plane_dir}
;;

type plane_states = plane_state list;;
let planes_fly t pstates : plane_states =
   List.map (plane_fly t) pstates;;

(* Asserting the evidence *)

(* First, the simplest case (which was implemented in AILog)
   We are given the set of blips that have been observed at given position.
   Nothing is said about the blips at other positions. That is,
   the given set of blips is not the exhaustive set; there may have been
   other blips.
*)

let blips_at_least (blips : pos list) (pstates : plane_states) =
   let check_blip blip = 
     flip 0.02 ||				(* randomly occurring blip *)
     List.fold_left (fun acc pstate -> acc || 
                                       (pstate.plane_pos = blip && flip 0.9))
            false pstates in
   List.iter (fun blip -> if check_blip blip then () else fail ()) blips
;;

(* The ideal case with no noise *)
let blips_at_least_ideal (blips : pos list) (pstates : plane_states) =
   let check_blip blip = 
     List.fold_left (fun acc pstate -> acc || pstate.plane_pos = blip)
            false pstates in
   List.iter (fun blip -> if check_blip blip then () else fail ()) blips
;;


(* Predict the observation at the current time moment: the blip
   given the states of the planes.
*)
   
(* Parameters of the model: 
    nplanes -- Some n: the maximal expected number of planes
               None  : the possible number of planes is unlimited
    ntimes  -- the number of time steps to run the model (the last
               time step)
    observations: a function that checks the evidence. It receives
            the time moment and plane_states
   outcomes: the function that computes the final outcome
*)

let blip_HMM_model nplanes ntimes evidence foutcome () =
   let np = match nplanes with
              | Some n -> geometric_bounded n 0.85
              | None   -> geometric 0.85 in
   let rec state0 = function 0 -> []
   | i -> plane_state_0 i :: state0 (pred i) in
   let rec iter t =
    let new_ps = 
      if t = 0 then state0 np
      else
	let ps = variable_elim iter (pred t) in
	planes_fly t ps
    in let () = evidence new_ps t in		(* check the evidence *)
    new_ps
  in foutcome (iter ntimes)
;;


(* Check the prob of evidence of two blips, at pos (3,3) and (3,4)
   at successive time moments *)

let two_blips ps = function 0 -> blips_at_least [(3,3)] ps
  | 1 -> blips_at_least [(3,4)] ps
  | _ -> ()
;;


(* a few tests first *)

let [(0.0100000000000000019, V ())] = 
  exact_reify (fun () -> 
    if (plane_state_0 1).plane_pos = (3,3) then () else fail ());;

let [(0.130434782608695676, V 1); (0.869565217391304324, V 0)] =
 exact_reify (fun () -> geometric_bounded 1 0.85);;

(* explanation: 0.02 +. 0.98 *. 0.00130434782608695684 *. 0.9;; *)
let [(0.0211504347826093099, V ())] =
 exact_reify (blip_HMM_model (Some 1) 0 
		(fun ps -> function 0 -> blips_at_least [(3,3)] ps)
		(fun _ -> ()))
;;

(* the result is the product of prob of one plane times prob of that
   plane be at pos (3,3) *)
let [(0.00130434782608695684, V ())] =
 exact_reify (blip_HMM_model (Some 1) 0 
		(fun ps -> function 0 -> blips_at_least_ideal [(3,3)] ps)
		(fun _ -> ()))
;;

(* Two planes at the same time *)
let [(0.0191897654584221797, V 2); (0.127931769722814503, V 1);
   (0.852878464818763282, V 0)] =
 exact_reify (fun () -> geometric_bounded 2 0.85);;

let [(0.000199999999999999901, V ())] =
   exact_reify
   (let evidence = blips_at_least_ideal [(3,3); (3,5)] in
	fun () -> evidence [plane_state_0 1; plane_state_0 2]);;
(* Indeed: plane 1 may be at (3,3) and plane 2 at (3,5) --
   or the other way around.
*)

let [] =
 exact_reify (blip_HMM_model (Some 1) 0 
		(fun ps -> function 0 -> 
		  blips_at_least_ideal [(3,3); (3,5)] ps)
		(fun _ -> ()))
;;

let [(3.83795309168443282e-06, V 2)] =
    exact_reify (blip_HMM_model (Some 2) 0
		(let ev = blips_at_least_ideal [(3,3); (3,5)]
                 in fun ps -> function 0 -> ev ps)
		(fun ps -> List.length ps))
;;
(* The exact result: prob of two planes is 
   0.0191897654584221797
   The computed result is correct
*)

(* Alas, already the following takes too long...
    exact_reify (blip_HMM_model (Some 3) 0
		(let ev = blips_at_least_ideal [(3,3); (3,5); (3,7)]
                 in fun ps -> function 0 -> ev ps)
		(fun ps -> List.length ps))
;;
*)


(* Planes in motion *)
let [(0.00059257469565211177, V ())] =
 exact_reify (blip_HMM_model (Some 1) 1
		two_blips
		(fun _ -> ()))
;;

let [(0.000626031812173919, V ())] =
 sample_importance (random_selector 17) 1000 (blip_HMM_model (Some 1) 1
		two_blips
		(fun _ -> ()))
;;

(* But already with two planes the model becomes too complex to solve exactly.
   We remove the variable elimination.
   It seems sampling 5000 worlds gives a good enough approximation, and
   it finishes within a second.
 *)


let blip_HMM_model nplanes ntimes evidence foutcome () =
   let np = match nplanes with
              | Some n -> geometric_bounded n 0.85
              | None   -> geometric 0.85 in
   let rec state0 = function 0 -> []
   | i -> plane_state_0 i :: state0 (pred i) in
   let rec iter t =
    let new_ps = 
      if t = 0 then state0 np
      else
	let ps = iter (pred t) in
	planes_fly t ps
    in let () = evidence new_ps t in		(* check the evidence *)
    new_ps
  in foutcome (iter ntimes)
;;

let [(0.000537223652173907085, V ())] =
 sample_importance (random_selector 17) 5000 (blip_HMM_model (Some 1) 1
		two_blips
		(fun _ -> ()))
;;

let [(0.000858385704719978691, V ())] =
 sample_importance (random_selector 17) 5000 (blip_HMM_model None 1
		two_blips
		(fun _ -> ()))
;;

(* This is the result we obtained in blip_ailog. But now we can show the
distribution in the number of planes. *)

let [(2.53840000000000075e-08, V 5); (7.1733848e-07, V 4);
     (2.89022758400000202e-05, V 3); (0.000126835698400000349, V 2);
     (0.000357885007999987398, V 1); (0.000344019999999996154, V 0)] =
 sample_importance (random_selector 17) 5000 (blip_HMM_model None 1
		two_blips
		(fun ps -> List.length ps))
;;


(* The exact result for the query below is 3.83795309168443282e-06
   As we increase the number of samples, the estimates converge to
   the exacty result. *)
let [(5.88486140724946778e-06, V 2)] =
    sample_importance (random_selector 17) 5000
      (blip_HMM_model (Some 2) 0
		(let ev = blips_at_least_ideal [(3,3); (3,5)]
                 in fun ps -> function 0 -> ev ps)
		(fun ps -> List.length ps))
;;

(* Alas, even with 10,000 samples, we don't get any result for three
   planes... We really should use laziness.
*)
let [] =
    sample_importance (random_selector 17) 5000
      (blip_HMM_model (Some 3) 0
		(let ev = blips_at_least_ideal [(3,3); (3,5); (3,7)]
                 in fun ps -> function 0 -> ev ps)
		(fun ps -> List.length ps))
;;

(* However, if noise is present, we get an estimate ... *)
let [(6.67493667269055322e-07, V 3); (9.12817797810127662e-07, V 2);
     (2.37663148208755515e-06, V 1); (4.19746996917189332e-06, V 0)] =
    sample_importance (random_selector 17) 5000
      (blip_HMM_model (Some 3) 0
		(let ev = blips_at_least [(3,3); (3,5); (3,7)]
                 in fun ps -> function 0 -> ev ps)
		(fun ps -> List.length ps))
;;
(* The exact result is:
   1e-6 *.  (* join prob of three planes being at three cells of the grid *)
   6.0 *.   (* 3! *)
   0.00287020304028914744;;(* prob that there are three planes, geom_bounded 3*)
   = 1.72212182417348842e-08

  With 25000 samples, the estimated prob of three planes is
   4.04041263569676812e-07. So we see a slow convergence to the exact
   result. Without noise, 25000 samples produce nothing.
*)

(* Let's try the example from Stuart Russell's talk: at each time moment, three
   marks are observed.
*)

let blip333 ps = function
  | 0 -> blips_at_least [(3,5); (3,7); (3,9)] ps
  | 1 -> blips_at_least [(4,5); (4,7); (4,9)] ps
  | 2 -> blips_at_least [(5,5); (5,7); (5,9)] ps
  | _ -> ()
;;

let (7.95996296929161396e-14,
     [(1.40953355226456531e-07, V 5); (0.0158591404344528564, V 4);
      (0.901408459486833902, V 3); (0.0250208630427655628, V 2);
      (0.0519967805529113342, V 1); (0.00571461552968104743, V 0)]) =
 Inference.normalize
    (sample_importance (random_selector 17) 5000 (blip_HMM_model None 2
		blip333
		(fun ps -> List.length ps)))
;;

(* Three planes seem to be most likely *)


(* Future work: modify HMM and evidence checking to return the index of 
   a plane which produced the observed blip. The model should accumulate these
   facts and return the distribution: essentially the probability of
   different identifications of the plane.
*)
