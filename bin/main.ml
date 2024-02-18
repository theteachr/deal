open Deal
open Minttea

let init _ = Command.Seq [ Enter_alt_screen; Hide_cursor ]

let update event game =
  match event with
  | Event.KeyDown Escape -> (game, Command.Quit)
  | Event.KeyDown Enter -> (Game.play game, Command.Noop)
  | Event.KeyDown (Key "p") -> (Game.pass game, Command.Noop)
  | Event.KeyDown (Key "j" | Down) ->
      (Game.choose_from_hand game Next, Command.Noop)
  | Event.KeyDown (Key "k" | Up) ->
      (Game.choose_from_hand game Prev, Command.Noop)
  | _ -> (game, Command.Noop)

let view Game.{ table = player, _; deck; turn; state } =
  let view_state =
    match state.choosing with
    | Game.State.Hand selected ->
        player.hand
        |> List.mapi (fun i card ->
               let bullet = if i = selected then ">" else " " in
               Printf.sprintf "%s %s" bullet (Card.display card))
        |> String.concat "\n"
  in
  let view_assets (player : Player.t) =
    let Player.Assets.{ bank; properties } = player.assets in
    let view_bank =
      bank
      |> List.map (function
           | Card.Money.Money value -> Printf.sprintf "(%d) Money" value
           | Card.Money.Action card ->
               Printf.sprintf "(%d) %s" (Card.Action.value card)
                 (Card.Action.name card))
      |> String.concat "\n"
    in
    let view_properties =
      properties
      |> Player.Assets.Properties.bindings
      |> List.map (fun (color, (properties, buildings)) ->
             let property_names =
               properties |> List.map Card.Property.name |> String.concat ", "
             in
             let buildings =
               buildings
               |> List.map (fun b -> Card.Action.(Building b |> name))
               |> String.concat ", "
             in
             let property_value =
               properties
               |> List.map Card.Property.value
               |> List.fold_left ( + ) 0
             in
             Format.sprintf "(%d) %s -> [%s] [%s]" property_value
               (Color.display color) property_names buildings)
      |> String.concat "\n"
    in
    Format.sprintf {|
%s

%s
|} view_bank view_properties
  in
  Format.sprintf
    {|

%s is playing (%d) [%d].

%s

%s

%d card(s) in the deck.
  |}
    player.name turn state.cards_played view_state (view_assets player)
    (Deck.count deck)

let deal = Minttea.app ~init ~update ~view ()

let () =
  let players =
    [ "theteachr"; "procrastination"; "oat"; "patate" ]
    |> List.map Player.create
  in
  let game = Game.create players in
  Minttea.start deal ~initial_model:(Game.start_turn game)
