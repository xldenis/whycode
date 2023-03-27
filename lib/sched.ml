open Why3

module Lwt_S : Controller_itp.Scheduler = struct
  open Linol_lwt

  let multiplier = 1
  let blocking = false

  (* [insert_idle_handler p f] inserts [f] as a new function to call
         on idle, with priority [p] *)
  let insert_idle_handler _ f =
    let rec promise () =
      let* () = Lwt_unix.sleep 0.0 in
      if f () then promise () else return ()
    in
    Linol_lwt.spawn (fun () -> promise ())

  (* [insert_timeout_handler ms t f] inserts [f] as a new function to call
         on timeout, with time step of [ms] and first call time as [t] *)
  let insert_timeout_handler ms (t : float) (f : unit -> bool) =
    let time = Unix.gettimeofday () in
    let sleep = t -. time in

    spawn (fun () ->
        let* () = Lwt_unix.sleep sleep in
        let rec promise () =
          if f () then
            let* () = Lwt_unix.sleep ms in
            promise ()
          else return ()
        in
        promise ())

  (* public function to register a task to run on idle *)
  let idle ~(prio : int) f = insert_idle_handler prio f

  (* public function to register a task to run on timeout *)
  let timeout ~ms f =
    assert (ms > 0);
    let ms = float ms /. 1000.0 in
    let time = Unix.gettimeofday () in
    insert_timeout_handler ms (time +. ms) f
end
