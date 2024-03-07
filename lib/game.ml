module Table = struct
  type t = Player.t * Player.t list

  let update player (_, opponents) = (player, opponents)

  let turn (current, opponents) =
    match opponents @ [ current ] with
    | current :: opponents -> (current, opponents)
    | _ -> failwith "should not be empty"
end

module State = struct
  type phase =
    | Play
    | Discard
    | Play_dual of {
        card : Card.Dual.t;
        value : int;
        colored : Card.Dual.colored;
      }
    | Play_wild of {
        colors : Color.t list;
        index : int;
      }

  type t = {
    cards_played : Card.t list;
    phase : phase;
    message : string;
    index : int;
  }

  let init = { cards_played = []; phase = Play; message = ""; index = 0 }
  let reset state = { init with message = state.message }
end

type t = {
  table : Table.t;
  deck : Deck.t;
  turn : int;
  state : State.t;
  discarded : Card.t list;
}

let next_index i l = (i + 1) mod List.length l
let prev_index i l = (i + List.length l - 1) mod List.length l

let select_next ({ table = player, _; state; _ } as game) =
  let state =
    match state.phase with
    | Play_dual props ->
        { state with phase = Play_dual { props with colored = Right } }
    | Play_wild props ->
        {
          state with
          phase =
            Play_wild { props with index = next_index props.index props.colors };
        }
    | _ -> { state with index = next_index state.index player.hand }
  in
  { game with state }

let select_prev ({ table = player, _; state; _ } as game) =
  let state =
    match state.phase with
    | Play_dual props ->
        { state with phase = Play_dual { props with colored = Left } }
    | Play_wild props ->
        {
          state with
          phase =
            Play_wild { props with index = prev_index props.index props.colors };
        }
    | _ -> { state with index = prev_index state.index player.hand }
  in
  { game with state }

let current_player { table = player, _; _ } = player

(* MODIFIERS *)
let draw_from_deck n game =
  (* TODO: Reshuffle discarded when [List.length cards <> n] *)
  let cards, deck = Deck.draw n game.deck in
  let player = Player.update_hand (current_player game) cards in
  { game with table = Table.update player game.table; deck }

let next game =
  let n = if Player.empty_hand (current_player game) then 5 else 2 in
  {
    (draw_from_deck n game) with
    turn = game.turn + 1;
    state = State.reset game.state;
  }


let pass game =
  let Player.{ hand; _ } = current_player game in
  let excess = List.length hand - 7 in
  if excess > 0 then
    let message =
      Printf.sprintf "Excess cards in your hand. You need to discard %d." excess
    in
    { game with state = { game.state with phase = Discard; message } }
  else next { game with table = Table.turn game.table }

let discard ({ table = player, _; _ } as game) =
  let card, player = Player.remove_from_hand game.state.index player in
  let game =
    {
      game with
      discarded = card :: game.discarded;
      table = Table.update player game.table;
      state =
        {
          game.state with
          message =
            Printf.sprintf "%s discarded [%s]." player.name (Card.display card);
          phase = Discard;
          index = 0;
        };
    }
  in
  if List.length player.hand = 7 then pass game else game

let play_property property ({ table = player, _; _ } as game) =
  match property with
  | Card.Property.Dual (({ colored = None; _ } as dual), value) ->
      {
        game with
        state =
          {
            game.state with
            phase = Play_dual { card = dual; value; colored = Left };
          };
      }
      |> Result.ok
  | Wild None ->
      {
        game with
        state =
          {
            game.state with
            phase = Play_wild { colors = Color.all; index = 0 };
          };
      }
      |> Result.ok
  | _ ->
      if Player.has_full_set (Card.Property.color property) player then
        Error `Full_set
      else
        {
          game with
          table = Table.update (Player.add_property property player) game.table;
          state =
            {
              cards_played = Card.Property property :: game.state.cards_played;
              phase = Play;
              message =
                Printf.sprintf "%s played [%s]." (current_player game).name
                  (Card.Property.display property);
              index = 0;
            };
        }
        |> Result.ok

let play_money amount game =
  {
    game with
    table =
      Table.update (current_player game |> Player.add_money amount) game.table;
    state =
      {
        cards_played = Card.Money amount :: game.state.cards_played;
        phase = Play;
        message =
          Printf.sprintf "%s played [%s]." (current_player game).name
            (Card.Money.display amount);
        index = 0;
      };
  }
  |> Result.ok

let play_pass_go card game =
  {
    (draw_from_deck 2 game) with
    state =
      {
        cards_played = card :: game.state.cards_played;
        phase = Play;
        message =
          Printf.sprintf "%s played [%s]." (current_player game).name
            (Card.display card);
        index = 0;
      };
  }
  |> Result.ok

let ( let* ) = Result.bind

let play_card card game =
  match card with
  | Card.Property card -> play_property card game
  | Money card -> play_money card game
  | Action Pass_go -> play_pass_go card game
  | Action _ -> Error (`Not_implemented "action")
  | Rent _ -> Error (`Not_implemented "rent")

let over { table = { assets; _ }, _; _ } =
  assets |> Player.Assets.full_property_sets |> List.length >= 3

let update game =
  if game |> over then
    {
      game with
      state =
        {
          game.state with
          message =
            Printf.sprintf "Game over. %s won." (current_player game).name;
        };
    }
  else
    match game.state.phase with
    | Play -> (
        if List.length game.state.cards_played = 3 then
          {
            game with
            state =
              {
                game.state with
                message = "Can't play any more cards in this turn.";
              };
          }
        else
          let card, player =
            Player.remove_from_hand game.state.index (current_player game)
          in
          match
            play_card card { game with table = Table.update player game.table }
          with
          | Ok game -> game
          | Error e ->
              let message =
                match e with
                | `Not_implemented component ->
                    Printf.sprintf "`%s` not implemented." component
                | `Full_set -> "You already have a full set for that color."
              in
              { game with state = { game.state with message } })
    | Discard -> discard game
    | Play_dual { card; colored; value } -> (
        let card = Card.(Property.Dual (Dual.choose colored card, value)) in
        match play_property card game with
        | Ok game -> game
        | Error e ->
            let message =
              match e with
              | `Full_set -> "You already have a full set for that color."
            in
            { game with state = { game.state with message } })
    | Play_wild { colors; index } -> (
        let card = Card.(Property.Wild (Some (List.nth colors index))) in
        match play_property card game with
        | Ok game -> game
        | Error e ->
            let message =
              match e with
              | `Full_set -> "You already have a full set for that color."
            in
            { game with state = { game.state with message } })

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
