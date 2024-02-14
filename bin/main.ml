open Deal
open Minttea

let init _ = Command.Noop

let update event game =
  match event with
  | Event.KeyDown Escape -> (game, Command.Quit)
  | Event.KeyDown Enter -> (Game.end_turn game, Command.Noop)
  | Event.KeyDown (Key "j") -> (Game.choose_next game, Command.Noop)
  | Event.KeyDown (Key "k") -> (Game.choose_prev game, Command.Noop)
  | _ -> (game, Command.Noop)

let view Game.{ table = player, _; deck; turn; state } =
  let view_state = function
    | Game.Choosing_from_hand selected ->
        player.hand
        |> List.mapi (fun i card ->
               let bullet = if i = selected then ">" else " " in
               Printf.sprintf "%s %s" bullet (Card.show card))
        |> String.concat "\n"
  in
  Format.sprintf {|

%s is playing (%d).

%s

%d card(s) in the deck.
  |}
    player.name turn (view_state state) (Deck.count deck)

let deal = Minttea.app ~init ~update ~view ()

let () =
  let players =
    [ "theteachr"; "procrastination"; "oat"; "patate" ]
    |> List.map Player.create
  in
  let game = Game.create players in
  Minttea.start deal ~initial_model:(Game.start_turn game)
