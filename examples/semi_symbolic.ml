open Byoppl
open Semi_symbolic

let print_dist =
  let rec print_dist_alinea n d =
    print_string (String.make (2*n) ' ');
    match d with
    | Beta (a,b) ->
       print_string "Beta(";
       begin
         match !a,!b with
         | V a, V b -> print_float a; print_char ','; print_float b
         | _ -> print_newline (); print_dist_alinea (n+1) !a; print_string (String.make (2*n) ' '); print_char ','; print_newline (); print_dist_alinea (n+1) !b; print_string (String.make (2*n) ' ')
       end;
       print_char ')'; print_newline ()
    | Bernoulli p ->
       print_string "Bernoulli(";
       begin
         match !p with
         | V p -> print_float p
         | _ -> print_newline (); print_dist_alinea (n+1) !p; print_string (String.make (2*n) ' ')
       end;
       print_char ')'; print_newline ()
    | Gaussian (mu,sigma) ->
       print_string "Normal(";
       begin
         match !mu,!sigma with
         | V mu, V sigma -> print_float mu; print_char ','; print_float sigma
         | _ -> print_newline (); print_dist_alinea (n+1) !mu; print_string (String.make (2*n) ' '); print_char ','; print_newline (); print_dist_alinea (n+1) !sigma; print_string (String.make (2*n) ' ')
       end;
       print_char ')'; print_newline ()
    | V v ->
       print_float v; print_newline ()
  in print_dist_alinea 0

(** Question 3 **)

let a = 2.
let b = 3.

let constant v = ref (V v)

let beta_bernoulli v =
  let p = sample (Beta (constant a,constant b)) in
  let () = observe (Bernoulli p) v in
  p

let gauss_gauss v =
  let mu = sample (Gaussian (constant 0.,constant 1.)) in
  let () = observe (Gaussian (mu,constant 1.)) v in
  mu

let () =
  let d1 = infer beta_bernoulli 1. in
  let d2 = infer gauss_gauss 1.2 in
  print_dist d1;
  print_dist d2

(** Question 4 **)

let bernoulli_gauss v =
  let p = sample (Bernoulli (constant 0.5)) in
  let () = observe (Gaussian (p,constant 1.)) v in
  p

let () =
  try
    let d = infer bernoulli_gauss 0.5 in
    print_dist d
  with Not_tractable -> print_string "The model is not tractable\n"
