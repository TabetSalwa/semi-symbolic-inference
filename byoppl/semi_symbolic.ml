(* B. Calcul symbolique *)

type dist =
  | Beta of node * node
  | Bernoulli of node
  | Gaussian of node * node
  | V of float
and node = dist ref

let rec draw = function
    | Beta (a,b) ->
       let a = draw !a in
       let b = draw !b in
       Distribution.draw (Distribution.beta ~a ~b)
    | Bernoulli p ->
       let p = draw !p in
       Distribution.draw (Distribution.bernoulli_float ~p)
    | Gaussian (mu,sigma) ->
       let mu = draw !mu in
       let sigma = draw !sigma in
       Distribution.draw (Distribution.gaussian ~mu ~sigma)
    | V v -> v

exception Not_tractable

(** Question 2 **)

let sample d : node = ref d

let observe d v =
  match d with
  | Bernoulli p ->
     begin
       match !p with
       | Beta (r,s) ->
          begin
            match !r,!s with
            | V a, V b -> r := V (a +. v);
                          s := V (b +. (1. -. v))
            | _ -> raise Not_tractable
          end
       | _ -> raise Not_tractable
     end
  | Gaussian (p,q) ->
     begin
       match !p,!q with
       | Gaussian (t,u), V sigma ->
          begin
            match !t,!u with
            | V mu0,V sigma0 -> let s = sigma ** (-2.) in
                                let s0 = sigma0 ** (-2.) in
                                t := V ((mu0*.s0 +. v*.s)/.(s0 +. s));
                                u := V (sqrt (1./.(s0 +. s)))
            | _ -> raise Not_tractable
          end
       | _ -> raise Not_tractable
     end
  | _ -> raise Not_tractable

let infer model v  =
  !(model v)

(* C. Inférence semi-symbolique *)

(** Question 5 **)

type prob = { id: int; scores : float array }

val infer : (prob -> 'a -> 'b) -> 'a -> 'b Distribution.t

val infer : (prob -> 'a -> node) -> 'a -> node Distribution.t

val sample : prob -> node Distribution.t -> node

val observe : prob -> node Distribution.t -> node -> unit

let infer ?(n = 1000) model  data =
    let scores = Array.make n 0. in
    let values = Array.init n (fun i -> model { scores; id = i } data) in
    Distribution.support ~values ~logits:scores

(** Question 6 **)

let value n =
  let v = draw !n in
  let () = n := V v in
  v

(** Question 7 **)

let sample _prob d = ref (V (draw d))

let factor prob s =
  prob.scores.(prob.id) <- prob.scores.(prob.id) +. s

let observe prob d v =
  

(** Question 8 **)


(* D. Evaluation *)

(** Question 9 **)

(** Question 10 **)


(* E. Extensions *)

(** Question 11 **)

(** Question 12 **)

(** Question 13 **)
