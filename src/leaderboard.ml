open! Core
open! Async


type t = {
  mutable list_scores : (int) list;
  mutable list_names : (string) list

}
[@@deriving sexp, fields]


let file_path = "/home/ubuntu/.snake_scores.sexp"

let load () : t Deferred.t = 
  let t_values_from_sexp = Reader.load_sexp_exn file_path t_of_sexp in
  t_values_from_sexp
;; 

let update ?(player_name = "Ines") ~score () =
  let%bind t = load () in
  t.list_scores <- t.list_scores @ [score];
  let%bind () = Writer.save_sexp file_path (sexp_of_t t) in
  (* let score_int = int_of_sexp score in
  let list t <- player_name, score_int *)
  return t
;;

let to_table t ~n =
  let sorted_t_list_increasing_order = List.rev (List.sort t.list_scores 
  ~compare:(fun a b -> Int.compare a b) ) in
  (* List.sort t.list_scores  , Int.compare*)
  let top_scores = List.take sorted_t_list_increasing_order n in
  (* Core.print_endline (Leaderboard.to_table t ) *)
  let top_scores_string = List.map top_scores ~f:(fun n -> Int.to_string n) in
  String.concat ~sep:", " top_scores_string
;;
