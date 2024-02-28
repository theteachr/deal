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
  {
    game with
    state = { state with index = next_index state.index player.hand };
  }

let select_prev ({ table = player, _; state; _ } as game) =
  {
    game with
    state = { state with index = prev_index state.index player.hand };
  }

let next ({ table = player, opponents; deck; turn; _ } as game) =
  let n = if Player.empty_hand player then 5 else 2 in
  let cards, deck = Deck.draw n deck in
  (* TODO: Reshuffle discarded when [List.length cards <> n] *)
  let player = Player.update_hand player cards in
  {
    game with
    table = (player, opponents);
    turn = turn + 1;
    state = State.reset game.state;
    deck;
  }

let current_player { table = player, _; _ } = player

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

let play_card card game =
  let player =
    match card with
    | Card.Property card -> Player.add_property card (current_player game)
    | Money card -> Player.add_money card (current_player game)
    | Action _ -> failwith "todo: play action card"
    | Rent _ -> failwith "todo: play rent card"
  in
  {
    game with
    table = Table.update player game.table;
    state =
      {
        cards_played = card :: game.state.cards_played;
        phase = Play;
        message =
          Printf.sprintf "%s played [%s]." player.name (Card.display card);
        index = 0;
      };
  }

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
    | Play ->
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
          play_card card { game with table = Table.update player game.table }
    | Discard -> discard game

let start players =
  let table =
    match players with
    | current :: opponents -> (current, opponents)
    | [] -> failwith "no players"
  in
  let deck = Deck.(shuffle default) in
  let rec distribute count ((player, rest) as table) deck =
    (* Emulate the real way of distributing the cards? *)
    if count = 0 then (table, deck)
    else
      let cards, deck = Deck.draw 5 deck in
      let player = Player.update_hand player cards in
      distribute (count - 1) (Table.turn (player, rest)) deck
  in
  let table, deck = distribute (List.length players) table deck in
  next { table; deck; turn = 0; state = State.init; discarded = [] }
