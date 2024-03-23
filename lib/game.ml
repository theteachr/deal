module Table = struct
  type t = Player.t * Player.t list

  let update player (_, opponents) = (player, opponents)
  let opponents (_, opponents) = opponents

  let turn (current, opponents) =
    match opponents @ [ current ] with
    | current :: opponents -> (current, opponents)
    | _ -> (current, [])
end

module State = struct
  type phase =
    | Play
    | Discard
    | Play_dual of {
        card : Card.Dual.t;
        value : int;
        colored : Card.Dual.colored; (* TODO: Remove this field *)
      }
    | Play_wild of {
        colors : Color.t list;
        index : int;
      }
    | Show_table
    | Collect_rent of {
        want : int;
        got : int;
        targets : Player.t * Player.t list;
      }
    | Play_action of {
        action : Card.Action.t;
        as_money : bool;
      }
  (* TODO: Rearrange cards *)

  type t = {
    cards_played : Card.t list;
    phase : phase;
    message : string option;
    index : int;
  }

  let init = { cards_played = []; phase = Play; message = None; index = 0 }
end

type t = {
  table : Table.t;
  deck : Deck.t;
  turn : int;
  state : State.t;
  discarded : Card.t list;
}

let next_index i max_value = (i + 1) mod max_value
let prev_index i max_value = (i + max_value - 1) mod max_value
let current_player { table = player, _; _ } = player

(* MODIFIERS *)
let set_message message game =
  { game with state = { game.state with message = Some message } }

let set_phase phase game = { game with state = { game.state with phase } }
let set_index index game = { game with state = { game.state with index } }

let select colored f ({ state; _ } as game) =
  match state.phase with
  | Play_dual props -> game |> set_phase @@ Play_dual { props with colored }
  | Play_wild props ->
      game
      |> set_phase
         @@ Play_wild
              { props with index = f props.index (List.length props.colors) }
  | Play_action a ->
      game |> set_phase @@ Play_action { a with as_money = not a.as_money }
  | _ ->
      game
      |> set_index @@ f state.index (List.length (current_player game).hand)

let select_next game = select Card.Dual.Right next_index game
let select_prev game = select Card.Dual.Left prev_index game

let draw_from_deck n game =
  let cards, deck =
    game.deck
    |> Deck.draw n
    |> Either.fold ~right:Fun.id ~left:(fun (cards, remaining) ->
           let drawn, deck =
             game.discarded |> Deck.of_list |> Deck.draw_max remaining
           in
           (List.rev_append cards drawn, deck))
  in
  let player = Player.update_hand (current_player game) cards in
  { game with table = Table.update player game.table; deck }

let next game =
  let n = if Player.empty_hand (current_player game) then 5 else 2 in
  { (draw_from_deck n game) with turn = game.turn + 1; state = State.init }

let pass ({ table = player, _; _ } as game) =
  if List.length player.hand > 7 then
    game
    |> set_phase Discard
    |> set_message
         (Printf.sprintf "Excess cards in your hand. You need to discard %d."
            (List.length player.hand - 7))
  else next { game with table = Table.turn game.table }

let discard ({ table = player, _; _ } as game) =
  let card, player = Player.remove_from_hand game.state.index player in
  {
    game with
    discarded = card :: game.discarded;
    table = Table.update player game.table;
    state = { game.state with phase = Discard; index = 0 };
  }
  |> pass

let play_property property ({ table = player, _; _ } as game) =
  match property with
  | Card.Property.Dual (({ colored = None; _ } as dual), value) ->
      set_phase (Play_dual { card = dual; value; colored = Left }) game
      |> Result.ok
  | Wild None ->
      set_phase (Play_wild { colors = Color.all; index = 0 }) game |> Result.ok
  | _ ->
      (* TODO: Allow the player to have two sets of the same color.
         Recently found out that we can own two different sets of the same
         color. *)
      if Player.has_full_set (Card.Property.color property) player then
        Error `Full_set
      else
        {
          game with
          table = Table.update (Player.add_property property player) game.table;
        }
        |> Result.ok

let play_money card game =
  {
    game with
    table =
      Table.update (current_player game |> Player.add_money card) game.table;
  }

let play_pass_go game =
  {
    (draw_from_deck 2 game) with
    state = { game.state with index = game.state.index + 2 };
  }

let play_birthday game =
  let targets = Table.opponents game.table in
  game
  |> set_phase
     @@ Collect_rent
          { want = 2; got = 0; targets = (List.hd targets, List.tl targets) }
  |> set_message
     @@ Printf.sprintf "%s has played the Birthday card. Everyone should pay 2."
          (current_player game).name
  |> Result.ok

let play_action action game =
  match action with
  | Card.Action.Pass_go -> Ok (play_pass_go game)
  | Birthday -> play_birthday game
  | action -> Error (`Not_implemented (Card.Action.name action))

let play_card game =
  let card = List.nth (current_player game).hand game.state.index in
  (match card with
  | Card.Property card -> play_property card game
  | Money card -> Ok (play_money card game)
  | Action action ->
      Ok (game |> set_phase @@ Play_action { action; as_money = false })
  | Rent _ -> Error (`Not_implemented "rent"))
  |> Result.fold
       ~error:(fun error ->
         let message =
           match error with
           | `Not_implemented component ->
               Printf.sprintf "`%s` not implemented." component
           | `Full_set -> "You already have a full set for that color."
         in
         set_message message game)
       ~ok:(fun game ->
         let card, player =
           Player.remove_from_hand game.state.index (current_player game)
         in
         {
           game with
           table = Table.update player game.table;
           state =
             {
               game.state with
               index = 0;
               cards_played = card :: game.state.cards_played;
               message =
                 Printf.sprintf "%s played %s." player.name (Card.display card)
                 |> Option.some;
             };
         })

let over { table = { assets; _ }, _; _ } =
  assets |> Player.Assets.full_property_sets |> List.length >= 3

let play game =
  if List.length game.state.cards_played = 3 then
    set_message "Can't play any more cards in this turn." game
  else play_card game

let play_wild_card game card =
  play_property card game
  |> Result.fold ~ok:(set_phase Play) ~error:(function `Full_set ->
         set_message "You already have a full set for that color." game)

let update game =
  match game.state.phase with
  | Play -> play game
  | Discard -> discard game
  | Play_dual { card; colored; value } ->
      Card.(Property.Dual (Dual.choose colored card, value))
      |> play_wild_card game
  | Play_wild { colors; index } ->
      Card.(Property.Wild (Some (List.nth colors index))) |> play_wild_card game
  | Show_table -> game
  | Collect_rent _ -> failwith "todo"
  | Play_action { action; as_money } ->
      if as_money then
        game |> play_money @@ Card.Money.Action action |> set_phase Play
      else
        game
        |> play_action action
        |> Result.map (set_phase Play)
        |> Result.get_ok

let turn game = { game with table = Table.turn game.table }

let start players =
  let table =
    match players with
    | current :: opponents -> (current, opponents)
    | [] -> failwith "no players"
  in
  let deck = Deck.(shuffle default) in
  let rec distribute ?(count = List.length players) game =
    (* Emulate the real way of distributing the cards? *)
    if count = 0 then game
    else distribute ~count:(count - 1) (draw_from_deck 5 game |> turn)
  in
  distribute { table; deck; turn = 0; state = State.init; discarded = [] }
  |> next

let show_table game = set_phase Show_table game

let back game =
  match game.state.phase with Show_table -> set_phase Play game | _ -> game
