external set_gate : bool -> unit = "caml_tpcg_set_gate" [@@noalloc]

external reset : unit -> unit = "caml_tpcg_reset" [@@noalloc]

external num_guards : unit -> int = "caml_tpcg_num_guards" [@@noalloc]

external num_hit : unit -> int = "caml_tpcg_num_hit" [@@noalloc]

external init_calls : unit -> int = "caml_tpcg_init_calls" [@@noalloc]

let[@inline never] classify x = if x > 3 then "big" else "small"

let[@inline never] name = function
  | 0 -> "zero"
  | 1 -> "one"
  | n when n < 0 -> "negative"
  | _ -> "many"

let[@inline never] rec sum acc n = if n <= 0 then acc else sum (acc + n) (n - 1)

let[@inline never] safe_div x y = try x / y with Division_by_zero -> 0

(* Many values live across probes, including floats, which the C callback is
   free to clobber in registers. *)
let[@inline never] mix a b c d =
  let x = (a *. 2.) +. b in
  let y = if c > 0 then x *. d else x -. d in
  let z = if Float.abs y > 10. then y /. 3. else y +. 1. in
  x +. y +. z +. a +. b +. d +. Float.of_int c

(* Number of distinct guards hit while running [f] with the gate open. *)
let hits f =
  reset ();
  set_gate true;
  let result = f () in
  set_gate false;
  let n = num_hit () in
  ignore (Sys.opaque_identity result);
  n

let () =
  Printf.printf "init calls: %d\n" (init_calls ());
  Printf.printf "guards registered: %b\n" (num_guards () > 0);
  reset ();
  ignore (Sys.opaque_identity (classify 1, name 2, sum 0 3, safe_div 1 0));
  Printf.printf "hits with the gate closed: %d\n" (num_hit ());
  let one_branch = hits (fun () -> classify 1) in
  let both_branches = hits (fun () -> classify 1 ^ classify 5) in
  Printf.printf "if: both arms cover more than one: %b\n"
    (both_branches > one_branch);
  let one_case = hits (fun () -> name 0) in
  let all_cases =
    hits (fun () -> String.concat "" [name 0; name 1; name (-1); name 7])
  in
  Printf.printf "match: all cases cover more than one: %b\n"
    (all_cases > one_case);
  let no_iteration = hits (fun () -> sum 0 0) in
  let iterations = hits (fun () -> sum 0 3) in
  Printf.printf "loop: iterating covers more than not: %b\n"
    (iterations > no_iteration);
  let no_raise = hits (fun () -> safe_div 6 3) in
  let raise_and_no_raise = hits (fun () -> safe_div 6 3 + safe_div 1 0) in
  Printf.printf "try: the handler adds coverage: %b\n"
    (raise_and_no_raise > no_raise);
  let expected = mix 1.5 2.5 3 4.5 +. mix 1.5 2.5 (-3) 40.5 in
  let actual = ref 0. in
  let (_ : int) =
    hits (fun () -> actual := mix 1.5 2.5 3 4.5 +. mix 1.5 2.5 (-3) 40.5)
  in
  Printf.printf "values preserved across probes: %b\n"
    (Float.equal expected !actual);
  Printf.printf "results: %s %s %d %d\n" (classify 5) (name (-2)) (sum 0 10)
    (safe_div 7 0)
