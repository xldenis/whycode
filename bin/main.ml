let run () =
  let s = new Server.why_lsp_server () in
  let server = Linol_lwt.Jsonrpc2.create_stdio s in
  let task : unit Lwt.t = Linol_lwt.Jsonrpc2.run server in

  (* let scheduler =
       Lwt_preemptive.detach
         (fun () ->
           (* Format.printf "Starting scheduler\n"; *)
           (* Format.print_flush (); *)
           Server.S.main_loop ~prompt:"" (fun _ -> ()))
         ()
     in *)
  match Lwt_main.run (Lwt.join [ task ]) with
  | () -> ()
  | exception e ->
      let e = Printexc.to_string e in
      Printf.eprintf "error: %s\n%!" e;
      exit 1

(* Finally, we actually run the server *)
let () = run ()
