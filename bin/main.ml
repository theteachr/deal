open Deal

let view_bank bank = bank |> List.map Card.Money.display |> String.concat "\n"

let view_selected items selected view =
  let view_item i card =
    let bullet = if i = selected then ">" else " " in
    Printf.sprintf "%s %s" bullet (view card)
  in
  items |> List.mapi view_item |> String.concat "\n"

let view_set (color, ((properties, buildings) as set)) =
  let property_names =
    properties |> List.map Card.Property.display |> String.concat ", "
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

let view_state (state : Game.State.t) (player : Player.t) =
  match state.phase with
  | Play ->
      let open Printf in
      let cards_played =
        state.cards_played |> List.map Card.display |> String.concat ", "
      in
      let header =
        if List.length state.cards_played = 3 then
          sprintf "%s has played all cards in the turn." player.name
        else sprintf "%s is playing." player.name
      in
      sprintf "%s\nCards turned: [%s]" header cards_played
  | Discard ->
      Printf.sprintf "%s has to discard %d." player.name
        (List.length player.hand - 7)

let view Game.{ table = player, _; deck; state; _ } =
  Format.sprintf
    {|
%s

%s

Bank:

%s

Properties:

%s

%d card(s) in the deck.

%s
|}
    (view_state state player)
    (* TODO: Don't view selectable hand when the player has already played
       3 cards. *)
    (view_selected player.hand state.index Card.display)
    (view_bank player.assets.bank)
    (view_properties player.assets.properties)
    (Deck.count deck) state.message

let clear_screen () =
  let _ = Sys.command "clear" in
  ()

let rec loop game =
  clear_screen ();
  print_endline (view game);
  if Game.over game then () else print_string "> ";
  Stdlib.(flush stdout);
  match Scanf.scanf " %s" Fun.id with
  (* quit *)
  | "q" -> "Game aborted." |> print_endline
  | "j" -> Game.select_next game |> loop
  | "k" -> Game.select_prev game |> loop
  (* finish turn *)
  | "f" -> Game.pass game |> loop
  (* play card *)
  | "p" -> Game.update game |> loop
  | _ -> game |> loop

let () =
  let players =
    List.map Player.create [ "theteachr"; "j"; "oat"; "patate"; "def" ]
  in
  loop (Game.start players)
(* Minttea.start deal ~initial_model:(Game.start players) *)
