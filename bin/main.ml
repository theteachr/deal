open Deal

let view_bank bank =
  bank
  |> List.map Card.Money.display_short
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

let view_dual Player.{ assets = { properties; _ }; _ }
    Card.Dual.{ colors = a, b; colored } =
  let color_a = Color.display a in
  let color_b = Color.display b in
  let selected =
    colored
    |> Option.map (function
         | Card.Dual.Left -> Printf.sprintf {|
> %s
  %s
|} color_a color_b
         | Right -> Printf.sprintf {|
  %s
> %s
|} color_a color_b)
    |> Option.value ~default:(Printf.sprintf "  %s\n  %s" color_a color_b)
  in
  Printf.sprintf {|
%s
%s
|} selected (view_properties properties)

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
    (Player.{ name; assets = { bank; properties }; _ }, next_targets) =
  Printf.sprintf
    {|
Got: %d
Want: %d
Target(s): %s [%s]

Bank: %s
Properties:
%s
|} got want
    name
    (next_targets
    |> List.map (fun Player.{ name; _ } -> name)
    |> String.concat "; ")
    (view_bank bank)
    (view_properties properties)

let view_play Game.{ table = player, _; state; _ } =
  Printf.sprintf {|
%s

Bank: %s
Properties:
%s|}
    (view_selected player.hand state.index Card.display)
    (view_bank player.assets.bank)
    (view_properties player.assets.properties)
(* TODO: Don't view selectable hand when the player has already played
   3 cards. *)

(* TODO: Let all static be grouped.
   Helps reduce the amount of moving things. *)
let view
    Game.(
      {
        state = { phase; index; message; cards_played; _ };
        table = player, opponents;
        deck;
        _;
      } as game) =
  let content =
    match phase with
    | Game.State.Show_table -> view_table opponents
    | Play -> view_play game
    | Discard -> view_discard player index
    | Play_dual { card; _ } -> view_dual player card
    | Play_wild { colors; index } -> view_wild player colors index
    | Collect_rent { got; want; targets } -> view_collect_rent got want targets
    | Play_action { as_money; _ } ->
        (* TODO: Move these static strings into a different place. *)
        if as_money then Printf.sprintf {|
  Play
> Use as money
|}
        else Printf.sprintf {|
> Play
  Use as money
|}
  in
  Printf.sprintf
    {|%s is playing.
This turn: [%s]
%s
%s
%d card(s) in the deck.|} player.name
    (cards_played |> List.map Card.display |> String.concat ", ")
    content
    (match message with
    | Some content -> Printf.sprintf "Message: %s" content
    | None -> "")
    (Deck.count deck)

let clear_screen () =
  let _ = Sys.command "clear" in
  ()

let rec loop game =
  clear_screen ();
  print_endline (view game);
  if Game.over game then
    Printf.printf "%s won!\n" (Game.current_player game).name
  else print_string "> ";
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
  (* FIXME: Pressing this when asking for rent; results in a loss of state *)
  | "v" -> Game.show_table game |> loop
  (* go back to play *)
  | "b" -> Game.back game |> loop
  | _ -> game |> loop

let () =
  let players =
    List.map Player.create [ "theteachr"; "j"; "oat"; "patate"; "def" ]
  in
  loop (Game.start players)
