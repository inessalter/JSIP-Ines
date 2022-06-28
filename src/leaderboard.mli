(** This module implements a leaderboard for the game. It saves scores to a
    file on the local disk. *)
open! Core

open Async

(** The type [t] here is abstract, but it should be thought of as holding the
    state about the players and their scores on the leaderboard. *)
type t

(** The [update] function accepts the name of a player (optionally) and the
    score. It should be called after each game ends to update the scores
    file. It asynchronously updates the leaderboard to use the provided
    score, and then returns the newly updated state of the
    leaderboard/scores. *)
val update : ?player_name:string -> score:int -> unit -> t Deferred.t
(*Update will need to be called after every single game, then, to_table will sort the scores and return the 
   specified number of top scores*)

(** The [to_table] function renders the scores for the top n players as a
    string. *)
val to_table : t -> n:int -> string
 (*Will sort the scores and return the specified number of top scores*) 