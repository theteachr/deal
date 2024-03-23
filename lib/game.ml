module Table = struct
  type t = Player.t * Player.t list

  let opponents (_, opponents) = opponents

  let turn (current, opponents) =
    match opponents @ [ current ] with
    | current :: opponents -> (current, opponents)
    | _ -> (current, [])
end

module State = struct
  (* XXX: This can be a type state instead of an enum *)
  type phase =
    | Play
    | Discard
    | Play_dual of {
        card : Card.Dual.t;
        value : int;
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

let update_player player game =
  { game with table = (player, Table.opponents game.table) }

let set_phase phase game = { game with state = { game.state with phase } }
let set_index index game = { game with state = { game.state with index } }
let reclaim card game = { game with discarded = card :: game.discarded }

let record_play card game =
  {
    game with
    state = { game.state with cards_played = card :: game.state.cards_played };
  }

let select f ({ state; _ } as game) =
  match state.phase with
  | Play_dual ({ card; _ } as props) ->
      game |> set_phase @@ Play_dual { props with card = Card.Dual.switch card }
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

let select_next = select next_index
let select_prev = select prev_index

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
  { (update_player player game) with deck }

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
  game |> reclaim card |> update_player player |> set_index 0 |> pass

let play_property property ({ table = player, _; _ } as game) =
  match property with
  | Card.Property.Dual (({ colored = None; _ } as dual), value) ->
      game
      |> set_phase @@ Play_dual { card = Card.Dual.choose Left dual; value }
  | Wild None ->
      game |> set_phase @@ Play_wild { colors = Color.all; index = 0 }
  | _ ->
      (* TODO: Handle playing a color that's already set.
         When the selected color is complete, the card should be part of a
         different set of the same color. *)
      game
      |> update_player (Player.add_property property player)
      |> set_phase Play

let play_money card game =
  game |> update_player (current_player game |> Player.add_money card)

let play_pass_go game =
  game |> draw_from_deck 2 |> set_index (game.state.index + 2) |> set_phase Play

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
  | Card.Property card -> Ok (play_property card game)
  | Money card -> Ok (play_money card game)
  | Action action ->
      (* XXX: This isn't quite right.
         We won't always be ok, because the player might choose to use the
         action in an invalid context. *)
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
         (* XXX: In some cases, immediately adding the card here isn't right.
            For instance, when the player is playing a wild property card,
            they will be choosing the color, and will not have played it
            yet. When we add it here, we won't be able to show the chosen
            color. *)
         game
         |> record_play card
         |> update_player player
         |> set_index 0
         |> set_message
            @@ Printf.sprintf "%s played %s." player.name (Card.display card))

let over { table = { assets; _ }, _; _ } =
  assets |> Player.Assets.full_property_sets |> List.length >= 3

let play game =
  if List.length game.state.cards_played = 3 then
    set_message "Can't play any more cards in this turn." game
  else play_card game

let update game =
  match game.state.phase with
  | Play -> play game
  | Discard -> discard game
  | Play_dual { card; value } ->
      let card = Card.(Property.Dual (card, value)) in
      play_property card game
  | Play_wild { colors; index } ->
      let color = List.nth colors index in
      let card = Card.(Property.Wild (Some color)) in
      play_property card game
  | Show_table -> game
  | Collect_rent _ -> failwith "todo"
  | Play_action { action; as_money } ->
      if as_money then
        game |> play_money @@ Card.Money.Action action |> set_phase Play
      else game |> play_action action |> Result.get_ok

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
