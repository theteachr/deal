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

%s is playing (%d).

-- hand --

%s

┌───────────┐
│           │
│           │
│           │
│   %3d     │
│           │
│           │
│ Deck      │
└───────────┘
  |}
    player.name turn
    (Player.Hand.to_string player.hand)
    (Deck.count deck)

let deal = Minttea.app ~init ~update ~view ()

let () =
  let players =
    [ "theteachr"; "procrastination"; "oat"; "patate" ]
    |> List.map Player.create
  in
  let game = Game.create players in
  Minttea.start deal ~initial_model:(Game.start_turn game)
