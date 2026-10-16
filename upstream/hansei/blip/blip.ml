(*
   Airplane-blip example from the BLOG papers of Brian Milch et al.
   This example was mentioned in Stuart Russell's talk at the
   NIPS2008 Probabilistic Programming workshop.
   See also example 1.3 of
   http://people.csail.mit.edu/milch/papers/blog-chapter.pdf

``An unknown number of aircraft exist in some volume of airspace. An
aircraft's state (position and velocity) at each time step depends on
its state at the previous time step. We observe the area with radar:
aircraft may appear as identical blips on a radar screen. Each blip
gives the approximate position of the aircraft that generated
it. However, some blips may be false detections, and some aircraft may
not be detected. What aircraft exist, and what are their trajectories?
Are there any aircraft that are not observed?'' [p4 of the above PDF]

   This example is meant to show-case open worlds and BLOG's ability
   to reason with open worlds.

We essentially reproduce the code from Fig 1.3 of the BLOG chapter.
Our model supports generalization: at each time step, certain planes
may disappear (go beyond radar detection) and certain new planes
may appear (come into range of radar detection). So, the number of the
planes does not have to stay constant during the evolution. That general
model was mentioned in Stuart Russel's talk at the NIPS workshop.
We try both models. Since the BLOG chapter does not specify various
priors in detail, we use parameters from AILog's implementation of a
simpler (2D rather than 3D) model.

In this code, we solve the problem with sequential Monte-Carlo,
using either exact inference or importance sampling at each time
step.


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

The previous models (blip_naive.ml) interpreted the evidence as
partial: if we are told of the observed blip at position (3,3) we
assume nothing is known about other positions on the radar
screen. There may be blips at the other positions, too -- but we are
not told about them.

It is more realistic to assume that the entire screen is observed at
once.  Thus the evidence of the blip at position (3,3) also implies
there are no blips anywhere else on the screen.

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
(* Initial (x,y) position of the plane i *)
let xpos0 i = uniform npos;;
let ypos0 i = uniform npos;;

(* Initial direction of the plane i *)
let dir0 i = uniformly [|North; East; South; West|];;

(* x- or y-Derivatives for the plane i at the time t travelling in the
   direction dir
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
type lpos = (unit -> int) * (unit -> int);; (* Lazily evaluated *)

(* ---------------  The equations of motion and radar detection *)

(* The state of each plane at a given time moment *)
type plane_state = {plane_idx : int;
                    plane_pos : pos;
                    plane_dir : dir};;

type lplane_state = {lplane_idx : int;	(* lazily evaluated *)
                     lplane_pos : lpos;
                     lplane_dir : unit -> dir};;

(* obtain a sample initial plane state, for the plane number i *)
let lplane_state_0 i =
   {lplane_idx = i; 
    lplane_pos = ((letlazy (fun () -> xpos0 i)), 
		  (letlazy (fun () -> ypos0 i))); 
    lplane_dir = letlazy (fun () -> dir0 i)};;

(* Evolve the state of one plane to the next time moment *)
let plane_fly t pstate : lplane_state =
   let i = pstate.lplane_idx in
   let (x,y) = pstate.lplane_pos in
   let newx ()  = 
     let x' = xderiv i t (pstate.lplane_dir ()) in
     let v = x () + x' in
     if v < 0 || v > 9 then fail () else v in
   let newy ()  = 
     let y' = yderiv i t (pstate.lplane_dir ()) in
     let v = y () + y' in
     if v < 0 || v > 9 then fail () else v in
   {lplane_idx = i; lplane_pos = (letlazy newx,letlazy newy); 
    lplane_dir = letlazy (fun () -> new_dir i t (pstate.lplane_dir ()))}
;;

(* Parameters of the system *)
type sys_parm = {
     (* A possibly stochastic function that removes a plane from the
	list of existing planes. This is to model the plane disappearance
	(e.g., going too low or too high for radar detection)
      *)
   sp_annihilate : lplane_state list -> lplane_state list;
     (* A possibly stochastic function that returns the number of new
	planes to create.
      *)
   sp_create : unit -> int;}

(* The state of the system *)
type sys_state = {			(* lazy *)
     (* A counter to assign the identity to newly created planes *)
   st_counter : int;
     (* The state of each existing plane *)
   st_planes : lplane_state list;
   }
;;

type sys_sstate = {			(* strict, i.e., fully evaluated *)
     (* A counter to assign identity to newly created planes *)
   st_scounter : int;
     (* The state of each existing plane *)
   st_splanes : plane_state list;
   }
;;

(* Conversions between lazy and strict states *)
let force_plane lstate =
   {plane_idx = lstate.lplane_idx; 
    plane_pos = (let (x,y) = lstate.lplane_pos in (x (), y ()));
    plane_dir = lstate.lplane_dir ()};;
  

let lazy_plane state =
   {lplane_idx = state.plane_idx; 
    lplane_pos = (let (x,y) = state.plane_pos in ((fun () -> x), fun () ->y));
    lplane_dir = fun () -> state.plane_dir};;
  

let lazy_state sstate =
  {st_counter = sstate.st_scounter;
   st_planes =  List.map lazy_plane sstate.st_splanes};;


(* The blip model itself *)
(* Arguments of the model: 
    parms  -- sys_parms of the evolution
    state  -- sys_state from the previous step
    timep  -- the time moment
    observations: a function that checks the evidence. It receives
            the time moment and plane_states
 The model returns the evolved sys_state
*)

let blip_HMM_step parms state timep evidence =
   let (cnt,planes) = (state.st_counter,state.st_planes) in 
   let planes = parms.sp_annihilate planes in
   let planes =  			(* evolve the remaining planes *)
     List.map (fun ps -> letlazy (fun () -> plane_fly timep ps)) planes in
   let np = parms.sp_create () in	(* perhaps add more planes *)
   let (cnt,planes) = if np = 0 then (cnt,planes) else
    let new_cnt = cnt + np in
    let rec st0loop acc i = 
      if i >= new_cnt then acc
      else st0loop (letlazy (fun () -> lplane_state_0 i)::acc) (succ i)
    in (new_cnt, st0loop planes cnt)
   in let () = evidence planes timep in		(* check the evidence *)
   {st_scounter = cnt; 
    st_splanes = List.map (fun th -> force_plane (th ())) planes}
;;

(* ---------------  The ideal case: no observation noise *)

(* Asserting the evidence *)

(* First is the ideal case: the ground truth. There is no observation noise. *)
(* The given list of blips are the all blips that are observed.
   There are no blips anywhere else on the screen. This case lets us
   test the equations of motion.
*)

let blips_ideal (blips : pos list) (pstates : (unit -> lplane_state) list) =
  let blips_obs = List.map (fun pos -> (pos, false)) blips in
  (* observe the plane at plane_pos. Account for the observation
     in blips_obs by toggling the flag. Fail if plane_pos does not
     correspond to any blip.
  *)
  let rec observe plane_pos = function [] -> fail ()
   | ((x,y) as pos,_)::rest when x = fst plane_pos () && y = snd plane_pos ()
     -> (pos,true)::rest
   | bp::rest -> bp :: observe plane_pos rest
  in
  let blips_obs_post =
   List.fold_left (fun blips_obs psl -> observe (psl ()).lplane_pos blips_obs)
   blips_obs pstates in
  (* Check that there are no blips unaccounted for *)
  List.iter (fun (_,flag) -> if not flag then fail ()) blips_obs_post
;;

(* A few commonly-used parameters *)
let parm_no = 				(* Keep the number of planes:    *)
   {sp_annihilate = (fun x -> x);	(* no planes appear or disappear *)
    sp_create = (fun () -> 0)};;

let parm_n n = {sp_annihilate = (fun x -> x);
                sp_create = (fun () -> geometric_bounded n 0.85)};;

let parm_only n = {sp_annihilate = (fun x -> x);
                   sp_create = (fun () -> n)};;

let state0 = {st_counter = 0; st_planes = []};;

(* a few tests first *)
let [(0.0100000000000000019, V ())] = 
  exact_reify (fun () -> 
    if (let (x,y) = (lplane_state_0 1).lplane_pos in x () = 3 && y () = 3)
	then () else fail ());;

let [(0.130434782608695676, V 1); (0.869565217391304324, V 0)] =
 exact_reify (fun () -> geometric_bounded 1 0.85);;

(* the result is the product of prob of one plane times prob of that
   plane be at pos (3,3) *)
let [(0.00130434782608695684, V ())] =
 exact_reify (fun () ->
   blip_HMM_step (parm_n 1) state0 0
   (fun ps -> function 0 -> blips_ideal [(3,3)] ps);
   ())
;;

(* Two planes at the same time *)
let [(0.0191897654584221797, V 2); (0.127931769722814503, V 1);
   (0.852878464818763282, V 0)] =
 exact_reify (fun () -> geometric_bounded 2 0.85);;

let [(0.000200000000000000064, V ())] =
   exact_reify
   (fun () ->
   blips_ideal [(3,3); (3,5)]
   [letlazy (fun () -> lplane_state_0 1); 
    letlazy (fun () -> lplane_state_0 2)]);;
(* Indeed: plane 1 may be at (3,3) and plane 2 at (3,5) --
   or the other way around.
*)

let [] =
 exact_reify (fun () ->
   blip_HMM_step (parm_n 1) state0 0
   (fun ps -> function 0 ->  blips_ideal [(3,3); (3,5)] ps);
   ())
;;

let [(3.83795309168443282e-06, V 2)] =
 exact_reify (fun () ->
   let st = blip_HMM_step (parm_n 2) state0 0
   (fun ps -> function 0 ->  blips_ideal [(3,3); (3,5)] ps) 
   in st.st_scounter)
;;
(* The exact result: prob of two planes is 
   0.0191897654584221797
   The computed result is correct
*)

(* The exact result for the query below is 3.83795309168443282e-06
   As we increase the number of samples, the estimates converge to
   the exact result. 
   Actually, with lazy evaluation we get the exact result already.
*)
let [(3.83795309168467253e-06, V 2)] =
 sample_importance (random_selector 17) 1000 (fun () ->
   let st = blip_HMM_step (parm_n 2) state0 0
   (fun ps -> function 0 ->  blips_ideal [(3,3); (3,5)] ps) 
   in st.st_scounter)
;;

(* before: for 5000 samples, we get [(3.53091684434968109e-06, V 2)]
   which is quite precise
*)


(* Three planes at the same time *)
let [(0.00287020304028914744, V 3); (0.0191346869352609812, V 2);
   (0.127564579568406528, V 1); (0.850430530456043332, V 0)] =
 exact_reify (fun () -> geometric_bounded 3 0.85);;

(* First of all, the observation of two blips is consistent with
   the existence of three planes: two planes could be very close
   (at the same position as far as the radar is concerned)
   For the query below, the analytical solution is:
    prob of two planes: P_geom(np=2) *. (0.01 ** 2.) *. 2.;;
    (the last factor is 2!)
    prob of three planes: P_geom(np=2) *. (0.01 ** 3.) *. 3. =
    0.00287020304028914744 *. (0.01 ** 3.) *. 6.;;
    where the last factor is 3!
   So, the result computed below is correct.
*)

let [(1.72212182417348577e-08, V 3); (3.8269373870522e-06, V 2)] =
 exact_reify (fun () ->
   let st = blip_HMM_step (parm_n 3) state0 0
   (fun ps -> function 0 ->  blips_ideal [(3,3); (3,5)] ps) 
   in st.st_scounter)
;;

let [(1.72212182417348577e-08, V 3)] =
 exact_reify (fun () ->
   let st = blip_HMM_step (parm_n 3) state0 0
   (fun ps -> function 0 ->  blips_ideal [(3,3); (3,5); (3,7)] ps) 
   in st.st_scounter)
;;
(* the result is 6. *. 0.01 ** 3. *. 0.00287020304028914744;;
   It is correct. Without laziness, we couldn't compute it at all.
*)

(* In blip_naive.ml, without laziness, we could not compute any estimate
   even with 10,000 samples.
   The computed result is quite precise.
*)
let [(1.64376528117359854e-08, V 3)] =
    sample_importance (random_selector 17) 5000 (fun () ->
   let st = blip_HMM_step (parm_n 3) state0 0
   (fun ps -> function 0 ->  blips_ideal [(3,3); (3,5); (3,7)] ps) 
   in st.st_scounter)
;;

(* Planes in motion *)

(* One plane *)

(* Check the prob of evidence of two blips, at pos (3,3) and (3,4)
   at successive time moments *)

let two_blips_ideal ps = function 
  | 0 -> blips_ideal [(3,3)] ps
  | 1 -> blips_ideal [(3,4)] ps
  | _ -> ()
;;

(* First we do within the same query *)
(* a plane at (3,3) (prob 0.01), has derivatives (0,1)
   flies North: chance 0.8 * 0.7
   flies East:  chance 0.2 * 0.1
   flies West:  chance 0.2 * 0.1
  Each initial direction is equally likely. The end result is
   0.130434782608695676 *. 0.01 *.
   (0.56 +. 0.02 +. 0.02) *. 0.25;;
  = 0.000195652173913043542
*)
let [(0.000195652173913043515, V ())] =
 exact_reify (fun () ->
   let st = blip_HMM_step (parm_n 1) state0 0 two_blips_ideal in
   let st = blip_HMM_step parm_no (lazy_state st) 1 two_blips_ideal in
   ())
;;
(* Indeed, exact_reify agrees. So, the equations of motion seem 
   to be programmed correctly...
*)

let [(0.000176139130434787552, V ())] =
 sample_importance (random_selector 17) 1000 (fun () ->
   let st = blip_HMM_step (parm_n 1) state0 0 two_blips_ideal in
   let st = blip_HMM_step parm_no (lazy_state st) 1 two_blips_ideal in
   ())
;;
(* In an earlier version, for 5000 samples, the result is
 let [(0.000161739130434782947, V ())]
*)

(* Now we do it step-by-step: sequential Monte-Carlo *)
(* the exact inference result does not change, as expected *)
let [(0.000195652173913043515, V ())] =
  let q2b_s0 = 
   exact_reify (fun () -> blip_HMM_step (parm_n 1) state0 0 two_blips_ideal) in
  exact_reify (fun () ->
   let st = lazy_state (reflect q2b_s0) in
   let st = blip_HMM_step parm_no st 1 two_blips_ideal in
   ())
;;

(* Three planes in motion *)

let blip333_ideal ps = function
  | 0 -> blips_ideal [(3,5); (3,7); (3,9)] ps
  | 1 -> blips_ideal [(4,5); (4,7); (4,9)] ps
  | 2 -> blips_ideal [(5,5); (5,7); (5,9)] ps
  | _ -> ()
;;

(* After the first step *)
let q3b_s0 = 
   exact_reify (fun () -> blip_HMM_step (parm_n 3) state0 0 blip333_ideal)
;;

(*
  [(4.4846922504517947e-11,
    V
     {st_counter = 3;
      st_planes =
       [{plane_idx = 2; plane_pos = (3, 9); plane_dir = West};
        {plane_idx = 1; plane_pos = (3, 7); plane_dir = West};
        {plane_idx = 0; plane_pos = (3, 5); plane_dir = West}]}); ...
*)
let 384 = List.length q3b_s0;;

(* Convert the distribution; re-collate values *)
let map_dist f pv =
   let folder pm (p,V v) = 
    PMap.insert_with (+.) (f v) p pm in
   let pm = List.fold_left folder PMap.empty pv in
   PMap.foldi (fun v p a -> (p,V v)::a) pm [];;

let [(1.72212182417348577e-08, V 3)] =
   map_dist (fun ps -> List.length ps.st_splanes) q3b_s0;;

(* The result is
   0.00287020304028914744 *. 6. *. (0.01 ** 3.);;
where the first number is Pr(np=3)_geom_boundded3_0.85
*)

(* Compute the states of the planes at the conclusion of time 1 *)
let q3b_s1 = 
  exact_reify (fun () ->
   let st = lazy_state (reflect q3b_s0) in
   blip_HMM_step parm_no st 1 blip333_ideal);;
let 384 = List.length q3b_s1;;

let [(5.81216115658552e-11, V 3)] =
   map_dist (fun ps -> List.length ps.st_splanes) q3b_s1;;

(* Compute the states of the planes at the conclusion of time 2 *)
let q3b_s2 = 
  exact_reify (fun () ->
   let st = lazy_state (reflect q3b_s1) in
   blip_HMM_step parm_no st 2 blip333_ideal);;
let 384 = List.length q3b_s2;;

let [(2.07608302413096791e-12, V 3)] =
   map_dist (fun ps -> List.length ps.st_splanes) q3b_s2;;

(* All the computations are fast. The number of the worlds is preserved!
   So each step takes the same time. This is because only one deterministic
   motion is consistent with the evidence of moving blips.
*)

(* Let us permit planes appearing out of the blue... 
The following works, but takes some time...
let q3b_s11 = 
  exact_reify (fun () ->
   let st = lazy_state (reflect q3b_s0) in
   blip_HMM_step (parm_n 1) st 1 blip333_ideal);;

let 7296 = List.length q3b_s11;;

let [(2.32570680483087277e-13, V 4); (5.05405317963957713e-11, V 3)] =
   map_dist (fun ps -> List.length ps.st_splanes) q3b_s11;;
*)


(* Let us try sampling, for the same problem *)
(* After the first step *)

let qs3b_s0 = 
   sample_importance (random_selector 17) 1500
   (fun () -> blip_HMM_step (parm_n 3) state0 0 blip333_ideal)
;;

let 276 = List.length qs3b_s0;;

let [(1.66356968215159076e-08, V 3)] =
   map_dist (fun ps -> List.length ps.st_splanes) qs3b_s0;;

(* The exact result is [(1.72212182417348577e-08, V 3)] *)

(* Compute the states of the planes at the conclusion of time 1 *)
let qs3b_s1 = 
   sample_importance (random_selector 17) 1500 (fun () ->
   let st = lazy_state (reflect qs3b_s0) in
   blip_HMM_step parm_no st 1 blip333_ideal);;
let 268 = List.length qs3b_s1;;

let [(4.33824898973106702e-11, V 3)] =
   map_dist (fun ps -> List.length ps.st_splanes) qs3b_s1;;
(* The exact result is
   5.81216115658552e-11
*)

let qs3b_s2 = 
   sample_importance (random_selector 17) 1500 (fun () ->
   let st = lazy_state (reflect qs3b_s1) in
   blip_HMM_step parm_no st 2 blip333_ideal);;
let 273 = List.length qs3b_s2;;

let [(1.41187106411182113e-12, V 3)] =
   map_dist (fun ps -> List.length ps.st_splanes) qs3b_s2;;

(* The exact result is:
 2.07608302413096791e-12
*)
(* Note the the number of sampled worlds stays roughly the same.
   That is a good sign, both for the accuracy and performance.
   Each time step thus requires roughly the same time.
*)


(* Let us permit planes appearing out of the blue... *)

let qs3b_s11 = 
   sample_importance (random_selector 17) 1500 (fun () ->
   let st = lazy_state (reflect qs3b_s0) in
   blip_HMM_step (parm_n 1) st 1 blip333_ideal);;
let 392 = List.length qs3b_s11;;

let [(1.44912873051982694e-13, V 4); (3.65866961828425864e-11, V 3)] =
   map_dist (fun ps -> List.length ps.st_splanes) qs3b_s11;;

(* The exact result is 
let [(2.32570680483087277e-13, V 4); (5.05405317963957713e-11, V 3)]
   map_dist (fun ps -> List.length ps.st_planes) q3b_s11;;
*)

(* Let us even permit planes appearing or disappearing. We permit
   plane0 to drop out with prob 0.15 *)
let qs3b_s12 = 
   let parm = {sp_annihilate = 
     (fun x -> if (flip 0.15) then (match x with (_::t) -> t) else x);
     sp_create = (fun () -> geometric_bounded 1 0.85)} in
   sample_importance (random_selector 17) 1500 (fun () ->
   let st = lazy_state (reflect qs3b_s0) in
   blip_HMM_step parm st 1 blip333_ideal);;

let 405 = List.length qs3b_s12;;

let [(7.93516084107581552e-14, V 4); (3.37729931735941775e-11, V 3)] =
   map_dist (fun ps -> List.length ps.st_splanes) qs3b_s12;;

(* 
   Without lazy coordinates, we had to do this workaround:
   We discussed the problem with sampling at the end of
   blip_investigation.ml
   Here we use the first solution suggested in that file, which
   is like variable elimination only nothing is collated and thus
   no variable eliminated.
   To be precise, to compute Pr(state | evidence)
   we compute
     sum{ Pr(state | evidence, NP=n) * Pr(NP=n) }
   where NP is the number of planes, and Pr(NP=n) is the prior
   for the number of planes (geometric distribution).
   For testing, rather than computing the whole sum above in one
   step, we compute the individual factors
      Pr(state | evidence, NP=n)
*)


(* ---------------  The real case, with the observation noise *)

(* The given list of blips are the all blips that are observed.
   There are no blips anywhere else on the screen.

   There is observation noise, characterized by:
    th_falarm_num : probability distribution for the number of
     false alarms (see line 12 of Fig 1.3 of BLOG chapter)
    spacial distribution of false alarms (see
      FalseAlarmDistrib() on line 12 of Fig 1.3). 
    probability of detection of a plane

   There may be several planes at the same position. Fig 1.3 first
   determines how many planes at the given position give rise to blips
   (line 11); for each such blip due to a plane, we compute its
   observed position (line 15). Since Figure 1.3 provides no details
   on these distributions, we defer to AILog's concrete example:
   we assume the fixed probability of detecting a plane. Along with
   AILog we further assume that for the detected plane, the plane
   position is the blip position. Since AILog (and hence us) operate
   on a discrete grid, the assumption seems reasonable. The choice
   of the fixed probability of detection implies that DetectionCPD()
   in Fig 1.3 is the binomial distribution. Again, absent any further details,
   that is as good an assumption as any other.
*)

  (* Spacial distributions for false alarms.
     Fig 1.3 does not specify it; we take it to be uniform.
   *)
let false_alarm_pos i = (letlazy (fun () -> uniform npos), 
                         letlazy (fun () -> uniform npos));;

let blips_noise th_falarm_num p_detection
                (blips : pos list) (pstates : (unit -> lplane_state) list) =
  let blips_obs = List.map (fun pos -> (pos, false)) blips in
  (* observe the plane at plane_pos. Account for the observation
     in blips_obs by toggling the flag. The argument plane_pos
     is the position of the _detected_ plane. Therefore,
     if we can't find any blip corresponding to plane_pos, we fail.
  *)
  let rec observe plane_pos = function [] -> fail ()
   | ((x,y) as pos,_)::rest when x = fst plane_pos () && y = snd plane_pos ()
     -> (pos,true)::rest
   | bp::rest -> bp :: observe plane_pos rest
  in
  (* For each plane of the true state, decide if that plane is
     detected. If so, check that the evidence contains the
     corresponding blip.
  *)
  let blips_obs =
   List.fold_left (fun blips_obs psl -> 
     if flip p_detection then observe (psl ()).lplane_pos blips_obs
     else blips_obs)
   blips_obs pstates in
  (* Now generate false alarms according to the given distribution *)
  (* and check they are accounted for in the evidence              *)
   let nfalarm = th_falarm_num () in
   let blips_obs = 
     let rec loop blips_obs = function 0 -> blips_obs
       | n -> loop (observe (false_alarm_pos n) blips_obs) (pred n)
     in loop blips_obs nfalarm in
  (* Check that all observed blips are accounted for *)
  List.iter (fun (_,flag) -> if not flag then fail ()) blips_obs
;;


(* Trying to reproduce the exact result by setting noise to be very low.
   The exact result is the product of prob of one plane times prob of that
   plane be at pos (3,3): 0.00130434782608695684
*)
let [(0.00130378408638106126, V ())]  =
 exact_reify (fun () ->
   blip_HMM_step (parm_n 1) state0 0
   (fun ps -> function 0 -> blips_noise
     (fun () -> geometric_bounded 2 0.9999) 0.999
   [(3,3)] ps);
   ())
;;

let [(0.00870869565230306293, V ())] =
 exact_reify (fun () ->
   blip_HMM_step (parm_n 1) state0 0
   (fun ps -> function 0 -> blips_noise
     (fun () -> 1) 0.9999999999
   [(3,3)] ps);
   ())
;;

(* explanation:
 Pr(NP=1) *. 0.01 *. 0.01 +.
 Pr(NP=0) *. 0.01
 = 
 0.130434782608695676 *. 0.01 *. 0.01 +.
 0.869565217391304324 *. 0.01;;
 
There is always a false alarm, and it is at 0.01
*)


let [(0.00352565217386055334, V ())] =
 exact_reify (fun () ->
   blip_HMM_step (parm_n 1) state0 0
   (fun ps -> function 0 -> blips_noise
     (fun () -> dist [(0.7,0); (0.3,1)]) 0.9999999999
   [(3,3)] ps);
   ())
;;
(* explanation:
0.130434782608695676 *. 0.01 *. 0.7 +.
0.130434782608695676 *. 0.01 *. 0.3 *. 0.01 +.
0.869565217391304324 *. 0.3 *. 0.01;;
= 0.00352565217391304347
*)


(* more realistic parameters *)
let [(0.00347308695652166592, V ())] =
 exact_reify (fun () ->
   blip_HMM_step (parm_n 1) state0 0
   (fun ps -> function 0 -> blips_noise
     (fun () -> dist [(0.7,0); (0.3,1)]) 0.9
   [(3,3)] ps);
   ())
;;

(* explanation: the observed blip at (3,3) may be
   -- 1 plane detected at 0.01, no false alarms
   -- 1 plane detected at 0.01, one false alarm at (3,3)
   -- 1 plane undetected, one false alarm at (3,3)
   -- 0 planes, one false alarm at (3,3)

 0.130434782608695676 *. 0.9 *. 0.01 *. 0.7 +.
 0.130434782608695676 *. 0.9 *. 0.01 *. 0.3 *. 0.01 +.
 0.130434782608695676 *. 0.1 *. 0.3 *. 0.01 +.
 0.869565217391304324 *. 0.3 *. 0.01;;
 = 0.00347308695652173878

So, it seems the model of noise is working, and we can compute the
result by hand in the simple cases.
*)

(* Now we try the example with three blips in motion *)


let blip333_noise th_num p_det ps = function
  | 0 -> blips_noise th_num p_det [(3,5); (3,7); (3,9)] ps
  | 1 -> blips_noise th_num p_det [(4,5); (4,7); (4,9)] ps
  | 2 -> blips_noise th_num p_det [(5,5); (5,7); (5,9)] ps
  | _ -> ()
;;

let run_model nplanes_prior evidence nsteps nsamples =
   let s0 = sample_importance (random_selector 17) nsamples
     (fun () -> blip_HMM_step nplanes_prior state0 0 evidence) in
   let rec loop results st_prev n =
     let () = Printf.printf "At the beginning of step %d, #worlds %d\n"
              n (List.length st_prev) in
     let results = 
       (map_dist (fun ps -> List.length ps.st_splanes) st_prev)::results in
     if n >= nsteps then List.rev results 
     else 
      let sn = sample_importance (random_selector 17) nsamples (fun () ->
           let st = lazy_state (reflect st_prev) in
           blip_HMM_step parm_no st n evidence) in
      loop results sn (succ n)
   in loop [] s0 1
;;

(* For comparison, the ideal case:

run_model (parm_only 3)
  (blip333_ideal) 
  3 1500
;;

sample_importance: done 1500 worlds
At the beginning of step 1, #worlds 380
sample_importance: done 1500 worlds
At the beginning of step 2, #worlds 311
sample_importance: done 1500 worlds
At the beginning of step 3, #worlds 297
- : (Ptypes.prob * int Ptypes.vc) list list =
[[(5.85600000000000204e-06, V 3)]; [(2.48935737359999516e-08, V 3)];
 [(8.15103660056502408e-10, V 3)]]
*)

(* Interestingly, although blip333_noise literally is the same
 as blip333_ideal when noise is non-existent -- for exact inference
map_dist (fun ps -> List.length ps.st_splanes)
 (exact_reify
     (fun () -> blip_HMM_step (parm_only 3) state0 0 
       blip333_ideal))
;;
(Ptypes.prob * int Ptypes.vc) list = [(6.0000000000000442e-06, V 3)]

map_dist (fun ps -> List.length ps.st_splanes)
 (exact_reify
     (fun () -> blip_HMM_step (parm_only 3) state0 0 
       (blip333_noise (fun () -> 0) 0.99999999999)))
;;
- : (Ptypes.prob * int Ptypes.vc) list = [(5.99999999981997177e-06, V 3)]

The additional flips in blip333_noise, even if hardly ever taken,
affect sample_importance. The results are not literally the same
(although very close of course).

map_dist (fun ps -> List.length ps.st_splanes)
 (sample_importance (random_selector 17) 1500
     (fun () -> blip_HMM_step (parm_only 3) state0 0 
       blip333_ideal))
;;
- : (Ptypes.prob * int Ptypes.vc) list = [(5.85600000000000204e-06, V 3)]

map_dist (fun ps -> List.length ps.st_splanes)
 (sample_importance (random_selector 17) 1500
     (fun () -> blip_HMM_step (parm_only 3) state0 0 
       (blip333_noise (fun () -> 0) 0.99999999999)))
;;
- : (Ptypes.prob * int Ptypes.vc) list = [(5.97000000007960544e-06, V 3)]
*)

(*
run_model (parm_only 3)
  (blip333_noise (fun () -> geometric_bounded 2 0.98) 0.9) 
  3 1500
;;

sample_importance: done 1500 worlds
At the beginning of step 1, #worlds 2651
sample_importance: done 1500 worlds
At the beginning of step 2, #worlds 1136
sample_importance: done 1500 worlds
At the beginning of step 3, #worlds 1087
- : (Ptypes.prob * int Ptypes.vc) list list =
[[(4.03469982435119075e-06, V 3)]; [(1.56620897464021146e-08, V 3)];
 [(3.71907500228527556e-10, V 3)]]

As the noise decreases, we see the convergence
run_model (parm_only 3)
  (blip333_noise (fun () -> geometric_bounded 2 0.99) 0.99) 
  3 1500
;;
sample_importance: done 1500 worlds
At the beginning of step 1, #worlds 1704
sample_importance: done 1500 worlds
At the beginning of step 2, #worlds 697
sample_importance: done 1500 worlds
At the beginning of step 3, #worlds 686
- : (Ptypes.prob * int Ptypes.vc) list list =
[[(6.06609537836754644e-06, V 3)]; [(2.17891855413127138e-08, V 3)];
 [(7.14286901532054574e-10, V 3)]]

*)

(*
(* Realistic runs: prob detection is 0.9, up to 4 false alarms could occur.
   Up to 7 planes may exist
 *)
run_model (parm_n 7)
  (blip333_noise (fun () -> geometric_bounded 4 0.98) 0.9) 
  3 15000
;;


sample_importance: done 15000 worlds
At the beginning of step 1, #worlds 1872
sample_importance: done 15000 worlds
At the beginning of step 2, #worlds 4819
sample_importance: done 15000 worlds
At the beginning of step 3, #worlds 3732
- : (Ptypes.prob * int Ptypes.vc) list list =
[[(1.24319554114076786e-12, V 6); (1.96951646305213758e-11, V 5);
  (6.90886315526840228e-10, V 4); (1.44358344182988179e-08, V 3);
  (1.87655431681365304e-09, V 2); (2.72203025713091348e-10, V 1)];
 [(3.39825560299358318e-13, V 4); (2.38425928023228161e-11, V 3);
  (5.81303972914320904e-15, V 2); (4.06022577441257521e-18, V 1)];
 [(8.02106463322915417e-15, V 4); (6.07222153561743126e-13, V 3)]]


The results seem to agree with the common sense: at step 0, a number of
planes are possible (although three seem more likely). At step 2, fewer planes
are possible. At step 3, we see only 3 and 4 planes. The observed blips
strongly favor the hypothesis that there are three planes.

*)

