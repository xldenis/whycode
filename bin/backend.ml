open Why3.Itp_server
open Why3.Itp_communication

module Scheduler = struct
  let multiplier = 1
  let blocking = false

  (* the private list of functions to call on idle, sorted higher
         priority first. *)
  let idle_handler : (int * (unit -> bool)) list ref = ref []

  (* [insert_idle_handler p f] inserts [f] as a new function to call
         on idle, with priority [p] *)
  let insert_idle_handler p f =
    let rec aux l =
      match l with
      | [] -> [ (p, f) ]
      | ((p1, _) as hd) :: rem -> if p > p1 then (p, f) :: l else hd :: aux rem
    in
    idle_handler := aux !idle_handler

  (* the private list of functions to call on timeout, sorted on
         earliest trigger time first. *)
  let timeout_handler : (float * float * (unit -> bool)) list ref = ref []

  (* [insert_timeout_handler ms t f] inserts [f] as a new function to call
         on timeout, with time step of [ms] and first call time as [t] *)
  let insert_timeout_handler ms t f =
    let rec aux l =
      match l with
      | [] -> [ (ms, t, f) ]
      | ((_, t1, _) as hd) :: rem -> if t < t1 then (ms, t, f) :: l else hd :: aux rem
    in
    timeout_handler := aux !timeout_handler

  (* public function to register a task to run on idle *)
  let idle ~(prio : int) f = insert_idle_handler prio f

  (* public function to register a task to run on timeout *)
  let timeout ~ms f =
    assert (ms > 0);
    let ms = float ms /. 1000.0 in
    let time = Unix.gettimeofday () in
    insert_timeout_handler ms (time +. ms) f
end

module Protocol = struct
  let notifications = ref []
  let notify n = notifications := n :: !notifications

  let get_notifications () =
    let l = !notifications in
    notifications := [];
    List.rev l

  let requests = ref []

  let push_request r =
    (* print_string "\npush_request\n\n"; *)
    requests := r :: !requests

  let get_requests () =
    let l = !requests in
    requests := [];
    (* Format.printf "outstanding requests %d\n" (List.length l); *)
    List.rev l
end

module S = Make (Scheduler) (Protocol)
