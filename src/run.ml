open! Core

(* This is the core logic that actually runs the game. We have implemented
   enough of this for you to get started, but feel free to read this file as
   a reference because you'll end up modifying it eventually. *)
let every seconds ~f ~stop =
  let open Async in
  let rec loop () =
    (*let ivar = Ivar.create () in*)
    if !stop
    then return ()
    else
      Clock.after (Time.Span.of_sec seconds)
      >>= fun () ->
      f ();
      loop ()
  in
  don't_wait_for (loop ())
;;

let read_keys ~game_over =
  let open Async in
  let reader, writer = Pipe.create () in
  let write_keys_to_pipe () =
    don't_wait_for (Pipe.write writer (Snake_graphics.read_key ()))
  in
  Clock_ns.every
    ~stop:(Ivar.read game_over)
    (Time_ns_unix.Span.create ~ms:100 ())
    write_keys_to_pipe;
  reader
  
;;

let handle_keys (game : Game.t) ~game_over =
  let open Async in
  (* Clock_ns.every ~stop:(Ivar.read game_over) (Time_ns_unix.Span.create
     ~ms:50 ()) *)
  let reader = read_keys ~game_over in
  Pipe.iter_without_pushback reader ~f:(fun ch ->
      match ch with
      | None -> ()
      | Some ch ->
        Game.handle_key game ch;
        Snake_graphics.render game)
;;

let handle_steps (game : Game.t) ~game_over =
  let open Async in
  Clock_ns.every
    ~stop:(Ivar.read game_over)
    (Time_ns_unix.Span.create ~ms:100 ())
    (fun () ->
      Game.step game;
      Snake_graphics.render game;
      match Game.game_state game with
      | Game_over _ | Win -> Ivar.fill game_over () 
      | In_progress -> ())
      (* Ivar.fill game_over () *)
;;

let run () =
  let open Async in
  let game = Snake_graphics.init_exn () in
  Snake_graphics.render game;
  let game_over = Ivar.create () in
  don't_wait_for (handle_keys game ~game_over);
  handle_steps game ~game_over;
  upon (Ivar.read game_over) (fun leaderboard ->
    let score_yellow = Game.score_yellow game in 
    let score_purple = Game.score_purple game in 
    print_string "Top 5 Yellow Scores : ";
    upon (Leaderboard.update ~score:score_yellow ()) 
    (fun updated_leaderboard ->
    (
    Core.print_endline (Leaderboard.to_table updated_leaderboard ~n:5)));
    upon (Leaderboard.update ~score:score_purple ()) (fun updated_leaderboard ->
    (print_string "Top 5 Purple Scores : ";
    Core.print_endline (Leaderboard.to_table updated_leaderboard ~n:5))))
;;


(*   print_int score_yellow;
  (* let score_purple = Game.score_purple game in *) *)