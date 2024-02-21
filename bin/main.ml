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

let view Game.{ table = player, _; deck; state; _ } =
  let view_state =
    match state.choosing with
    | Game.State.Hand selected | Discarding selected ->
        player.hand
        |> List.mapi (fun i card ->
               let bullet = if i = selected then ">" else " " in
               Printf.sprintf "%s %s" bullet (Card.display card))
        |> String.concat "\n"
  in
  let view_bank bank =
    bank
    |> List.map (function
         | Card.Money.Money value -> Printf.sprintf "(%d) Money" value
         | Card.Money.Action card ->
             Printf.sprintf "(%d) %s" (Card.Action.value card)
               (Card.Action.name card))
    |> String.concat "\n"
  in
  let view_properties properties =
    properties
    |> Player.Assets.Properties.bindings
    |> List.map (fun (color, ((properties, buildings) as set)) ->
           let property_names =
             properties |> List.map Card.Property.name |> String.concat ", "
           in
           let buildings =
             buildings
             |> List.map (fun b -> Card.Action.(Building b |> name))
             |> String.concat ", "
           in
           Format.sprintf "[%2d] %s -> [%s] [%s]"
             (Player.Assets.rent color set)
             (Color.display color) property_names buildings)
    |> String.concat "\n"
  in
  Format.sprintf
    {|
%s is playing [can play %d more card(s)].

Hand (%d):

%s

Bank (%d):

%s

Properties:

%s

%d card(s) in the deck.

%s
|}
    player.name (3 - state.cards_played) (List.length player.hand) view_state
    (Player.Bank.value player.assets.bank)
    (view_bank player.assets.bank)
    (view_properties player.assets.properties)
    (Deck.count deck) state.message

let deal = Minttea.app ~init ~update ~view ()

let () =
  let players =
    [ "theteachr"; "j"; "oat"; "patate" ] |> List.map Player.create
  in
  Minttea.start deal ~initial_model:(Game.start players)
