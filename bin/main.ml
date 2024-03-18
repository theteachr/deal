open Deal

let view_bank bank =
  bank
  |> List.map (fun card -> string_of_int @@ Card.Money.value card)
  |> String.concat ", "
  |> Printf.sprintf "[%s]"

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

let view_dual Player.{ name; assets = { properties; _ }; _ }
    Card.Dual.{ colors = a, b; _ } colored =
  let color_a = Color.display a in
  let color_b = Color.display b in
  let selected =
    match colored with
    | Card.Dual.Left -> Printf.sprintf {|
> %s
  %s
|} color_a color_b
    | Right -> Printf.sprintf {|
  %s
> %s
|} color_a color_b
  in
  Printf.sprintf {|
%s is playing a dual card.
%s
%s
|} name selected
    (view_properties properties)

let view_table players =
  players
  |> List.map (fun Player.{ name; assets; _ } ->
         Printf.sprintf {|== %s ==
Bank value: %u
Properties:
%s
|} name
           (Player.Bank.value assets.bank)
           (view_properties assets.properties))
  |> String.concat "\n"

let view_discard Player.{ name; hand; _ } index =
  Printf.sprintf {|
%s has to discard %d.

%s
|} name
    (List.length hand - 7)
    (view_selected hand index Card.display)

let view_wild Player.{ name; assets = { properties; _ }; _ } colors index =
  Printf.sprintf {|
%s is playing a wild card.

%s

%s
|} name
    (view_selected colors index Color.display)
    (view_properties properties)

let view_collect_rent got want
    (Player.{ name; assets = { bank; properties }; _ }, next_targets) card =
  Printf.sprintf
    {|
Got: %d
Want: %d
Target(s): %s [%s]
Card: %s

Bank: %s
Properties:
%s
|}
    got want name
    (next_targets
    |> List.map (fun Player.{ name; _ } -> name)
    |> String.concat "; ")
    (Card.display card) (view_bank bank)
    (view_properties properties)

let view_play Game.{ table = player, _; deck; state; _ } =
  let cards_turned =
    state.cards_played |> List.map Card.display |> String.concat ", "
  in
  let phrase =
    if List.length state.cards_played = 3 then
      "has played all cards in the turn."
    else "is playing."
  in
  Printf.sprintf
    {|
%s %s
This turn: [%s]

%s

Bank: %s
Properties:
%s

%d card(s) in the deck.|}
    player.name phrase cards_turned
    (view_selected player.hand state.index Card.display)
    (view_bank player.assets.bank)
    (view_properties player.assets.properties)
    (* TODO: Don't view selectable hand when the player has already played
       3 cards. *)
    (Deck.count deck)

let view
    Game.(
      { state = { phase; index; message; _ }; table = player, opponents; _ } as
      game) =
  let content =
    match phase with
    | Game.State.Show_table -> view_table opponents
    | Play -> view_play game
    | Discard -> view_discard player index
    | Play_dual { card; colored; _ } -> view_dual player card colored
    | Play_wild { colors; index } -> view_wild player colors index
    | Collect_rent { got; want; card; targets } ->
        view_collect_rent got want targets card
  in
  Printf.sprintf {|%s
%s|} content
    (match message with
    | Some message -> Printf.sprintf "Message: %s" message
    | None -> "")

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
  | "v" -> Game.show_table game |> loop
  (* go back to play *)
  | "b" -> Game.back game |> loop
  | _ -> game |> loop

let () =
  let players =
    List.map Player.create [ "theteachr"; "j"; "oat"; "patate"; "def" ]
  in
  loop (Game.start players)
(* Minttea.start deal ~initial_model:(Game.start players) *)
