open! Core

type t =
  | In_progress
  | Game_over of string
  | Win
[@@deriving sexp_of, compare]

let to_string t =
  match t with In_progress -> "" | Game_over x -> " " ^ x | Win -> "WIN!"
;;

(*let original_position t { Position.row; col } : Position.t = match t with |
  In_progress -> { row; col } | Game_over _ ->{ row; col } | Win -> { row;
  col } | Restart -> { row = 0; col = 0 } ;;*)
