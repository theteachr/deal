open Deal
open Minttea

let _init _ = Command.Seq [ Enter_alt_screen; Hide_cursor ]

let _update event game =
  match event with
  | Event.KeyDown Escape -> (game, Command.Quit)
  | Event.KeyDown Enter -> (Game.update game, Command.Noop)
  | Event.KeyDown (Key "p") -> (Game.pass game, Command.Noop)
  | Event.KeyDown (Key "j" | Down) -> (Game.select_next game Next, Command.Noop)
  | Event.KeyDown (Key "k" | Up) -> (Game.select_next game Prev, Command.Noop)
  | _ -> (game, Command.Noop)

let view_bank bank = bank |> List.map Card.Money.display |> String.concat "\n"

let view_hand hand selected =
  let view_card i card =
    let bullet = if i = selected then ">" else " " in
    Printf.sprintf "%s %s" bullet (Card.display card)
  in
  hand |> List.mapi view_card |> String.concat "\n"

let view_set (color, ((properties, buildings) as set)) =
  let property_names =
    properties |> List.map Card.Property.name |> String.concat ", "
  in
  let building_names =
    buildings
    |> List.map (fun b -> Card.Action.(Building b |> name))
    |> String.concat ", "
  in
  Format.sprintf "[%2d] %s -> [%s] [%s]"
    (Player.Assets.rent color set)
    (Color.display color) property_names building_names

let view_properties properties =
  properties
  |> Player.Assets.Properties.bindings
  |> List.map view_set
  |> String.concat "\n"

let view Game.{ table = player, _; deck; state; _ } =
  let current_player_status =
    match state.phase with
    | Play ->
        Printf.sprintf "%s is playing [can play %d more card(s)]." player.name
          (3 - state.cards_played)
    | Discard ->
        Printf.sprintf "%s has to discard %d." player.name
          (List.length player.hand - 7)
  in
  Format.sprintf
    {|
%s

Hand (%d):

%s

Bank:

%s

Properties:

%s

%d card(s) in the deck.

%s
|}
    current_player_status (List.length player.hand)
    (view_hand player.hand state.index)
    (view_bank player.assets.bank)
    (view_properties player.assets.properties)
    (Deck.count deck) state.message

let _deal = Minttea.app ~init:_init ~update:_update ~view ()

let rec loop game =
  (* TODO: Clear screen *)
  print_endline (view game);
  if Game.over game then ()
  else
    print_string "> ";
    Stdlib.flush Stdlib.stdout;
    match Scanf.scanf " %s" Fun.id with
    | "q" -> "Game aborted." |> print_endline
    | "j" -> Game.select_next game Next |> loop
    | "k" -> Game.select_next game Prev |> loop
    | "p" -> Game.pass game |> loop
    | "play" -> Game.update game |> loop
    | _ -> game |> loop

let () =
  let players =
    List.map Player.create [ "theteachr"; "j"; "oat"; "patate"; "def" ]
  in
  loop (Game.start players)
(* Minttea.start deal ~initial_model:(Game.start players) *)
