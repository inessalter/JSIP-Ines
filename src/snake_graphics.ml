open! Core

module Colors = struct
  let black = Graphics.rgb 000 000 000
  let green = Graphics.rgb 000 255 000
  let head_color = Graphics.rgb 220 046 046
  let red = Graphics.rgb 255 000 000
  let orange = Graphics.rgb 255 127 000
  let purple = Graphics.rgb 148 000 211
  let white = Graphics.rgb 255 255 255
  let gold = Graphics.rgb 255 223 000
  let sky = Graphics.rgb 000 000 255
  let dark_purple = Graphics.rgb 075 000 130
  let game_in_progress = Graphics.rgb 000 255 009
  let game_lost = Graphics.rgb 200 100 100
  let game_won = Graphics.rgb 100 200 100

  
  let apple_color apple =
    match Apple.color apple with
    | Red -> red
    | Gold -> gold
    | Sky -> sky
    | White -> white
    | Orange -> orange
    | Green -> green
    | Purple -> purple
    | Dark_Purple -> dark_purple
  ;;
end

module Constants = struct
  let play_area_height = 800
  let header_height = 100
  let play_area_width = 900
  let block_size = 36
end

let only_one : bool ref = ref false

let init_exn () =
  let open Constants in
  (* Should raise if called twice *)
  if !only_one
  then failwith "Can only call init_exn once"
  else only_one := true;
  Graphics.open_graph
    (Printf.sprintf
       " %dx%d"
       (play_area_height + header_height)
       play_area_width);
  let height = play_area_height / block_size in
  let width = play_area_width / block_size in
  Game.create ~height ~width ~initial_snake_length:3
;;

let draw_block { Position.row; col } ~color =
  let open Constants in
  let col = col * block_size in
  let row = row * block_size in
  Graphics.set_color color;
  Graphics.fill_rect (col + 1) (row + 1) (block_size - 1) (block_size - 1)
;;

let draw_header
    ~game_state
    ~score_yellow
    ~score_purple
  =
  let open Constants in
  let header_color =
    match (game_state : Game_state.t) with
    | In_progress -> Colors.game_in_progress
    | Game_over _ -> Colors.game_lost
    | Win -> Colors.game_won
  in
  Graphics.set_color header_color;
  Graphics.fill_rect 0 play_area_height play_area_width header_height;
  let header_text = Game_state.to_string game_state in
  Graphics.set_color Colors.black;
  Graphics.set_text_size 40;
  Graphics.moveto 0 (play_area_height + 50);
  Graphics.draw_string (Printf.sprintf " %s" header_text);
  Graphics.moveto (play_area_width - 300) (play_area_height + 60);
  Graphics.draw_string (Printf.sprintf "Purple Player:");
  Graphics.moveto (play_area_width - 300) (play_area_height + 30);
  Graphics.draw_string (Printf.sprintf "Score: %d" score_purple);
  Graphics.moveto (play_area_width - 600) (play_area_height + 60);
  Graphics.draw_string (Printf.sprintf "Yellow Player:");
  Graphics.moveto (play_area_width - 600) (play_area_height + 30);
  Graphics.draw_string (Printf.sprintf "Score: %d" score_yellow);
;;

let draw_play_area () =
  let open Constants in
  Graphics.set_color Colors.white;
  Graphics.fill_rect 0 0 play_area_width play_area_height;
  (*Horizontal Lines*)
  Graphics.set_color Colors.sky;
  Graphics.fill_rect 0 50 play_area_width 50;
  Graphics.set_color Colors.sky;
  Graphics.fill_rect 0 150 play_area_width 50;
  Graphics.set_color Colors.sky;
  Graphics.fill_rect 0 250 play_area_width 50;
  Graphics.set_color Colors.sky;
  Graphics.fill_rect 0 350 play_area_width 50;
  Graphics.set_color Colors.sky;
  Graphics.fill_rect 0 450 play_area_width 50;
  Graphics.set_color Colors.sky;
  Graphics.fill_rect 0 550 play_area_width 50;
  Graphics.set_color Colors.sky;
  Graphics.fill_rect 0 650 play_area_width 50;
  (*Vertical Lines*)
  Graphics.set_color Colors.sky;
  Graphics.fill_rect 50 0 play_area_width 50;
  Graphics.set_color Colors.sky;
  Graphics.fill_rect 150 0 50 play_area_height;
  Graphics.set_color Colors.sky;
  Graphics.fill_rect 250 0 50 play_area_height;
  Graphics.set_color Colors.sky;
  Graphics.fill_rect 350 0 50 play_area_height;
  Graphics.set_color Colors.sky;
  Graphics.fill_rect 450 0 50 play_area_height;
  Graphics.set_color Colors.sky;
  Graphics.fill_rect 550 0 50 play_area_height;
  Graphics.set_color Colors.sky;
  Graphics.fill_rect 650 0 50 play_area_height
;;

let draw_apple apple =
  let apple_location = Apple.location apple in
  draw_block apple_location ~color:(Colors.apple_color apple)
;;

let draw_snake snake_head snake_tail =
  List.iter snake_tail ~f:(draw_block ~color:Colors.gold);
  (* Snake head is a different color *)
  draw_block ~color:Colors.head_color snake_head
;;

let draw_snake_two snake_head snake_tail =
  List.iter snake_tail ~f:(draw_block ~color:Colors.purple);
  (* Snake head is a different color *)
  draw_block ~color:Colors.head_color snake_head
;;

let render game =
  Graphics.display_mode false;
  let snake = Game.snake game in
  let apple = Game.apple game in
  let second_snake = Game.second_snake game in
  let score_yellow = Game.score_yellow game in
  let score_purple = Game.score_purple game in
  let game_state = Game.game_state game in
  draw_header
    ~game_state
    ~score_yellow
    ~score_purple;
  draw_play_area ();
  draw_apple apple;
  draw_snake (Snake.head snake) (Snake.tail snake);
  draw_snake_two (Snake.head second_snake) (Snake.tail second_snake);
  Graphics.display_mode true;
  Graphics.synchronize ()
;;

let read_key () =
  if Graphics.key_pressed () then Some (Graphics.read_key ()) else None
;;
