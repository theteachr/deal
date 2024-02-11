open Deal
open Minttea

let init _ = Command.Noop

let update event game =
  match event with
  | Event.KeyDown Escape -> (game, Command.Quit)
  | Event.KeyDown (Key "p") -> (Game.end_turn game, Command.Noop)
  | _ -> (game, Command.Noop)

let view Game.{ table = { current = player; _ }; deck; turn } =
  Format.sprintf
    {|

%s's turn (%d).
%d card(s) left in the deck.

-- hand --

%s

  |}
    player.name turn (Deck.count deck)
    (Player.Hand.to_string player.hand)

let deal = Minttea.app ~init ~update ~view ()

let () =
  let players =
    [ "theteachr"; "procrastination"; "oat"; "patate" ]
    |> List.map Player.create
  in
  let game = Game.(create players |> start_turn) in
  Minttea.start deal ~initial_model:game
