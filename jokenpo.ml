type hands =
  | Stone
  | Paper
  | Scissors

type players =
  | P1
  | P2

type jokenpo_result =
  | Player of players
  | Draw

let play_jokenpo p1 p2 =
  match p1, p2 with
  (* draw *)
  | Stone, Stone -> Draw
  | Paper, Paper -> Draw
  | Scissors, Scissors -> Draw
  (* player 1 wins *)
  | Stone, Scissors -> Player P1
  | Paper, Stone -> Player P1
  | Scissors, Paper -> Player P1
  (* player 2 wins *)
  | Scissors, Stone -> Player P2
  | Stone, Paper -> Player P2
  | Paper, Scissors -> Player P2
;;

let check_input input =
  match input with
  | "1" -> Some Stone
  | "2" -> Some Paper
  | "3" -> Some Scissors
  | _ -> None
;;

let string_of_hand hand =
  match hand with
  | Stone -> "Stone"
  | Paper -> "Paper"
  | Scissors -> "Scissors"
;;

let rec read_hand input =
  match input with
  | Some mode ->
    print_endline ("You choose " ^ string_of_hand mode);
    mode
  | None -> read_hand (check_input (read_line ()))
;;

let show_hands_to player =
  let player =
    match player with
    | P1 -> "Player 1"
    | P2 -> "Player 2"
  in
  print_endline (player ^ "! Choose your hand");
  print_endline "(1) - Stone";
  print_endline "(2) - Paper";
  print_endline "(3) - Scissors"
;;

show_hands_to P1

let player1_hand = read_hand (check_input (read_line ()));;

show_hands_to P2

let player2_hand = read_hand (check_input (read_line ()))

let result = play_jokenpo player1_hand player2_hand

let result_message =
  match result with
  | Player P1 -> "Player 1 wins"
  | Player P2 -> "Player 2 wins"
  | Draw -> "Draw!"
;;

let () = print_endline result_message