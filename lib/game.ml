module Table = struct
  type t = Player.t * Player.t list

  let update player (_, opponents) = (player, opponents)

  let turn (current, opponents) =
    match opponents @ [ current ] with
    | current :: opponents -> (current, opponents)
    | _ -> failwith "should not be empty"
end

module State = struct
  type choosing =
    | Hand of int
    | Discarding of int

  type t = {
    cards_played : int;
    choosing : choosing;
    message : string;
  }

  let start = { cards_played = 0; choosing = Hand 0; message = "" }
end

type t = {
  table : Table.t;
  deck : Deck.t;
  turn : int;
  state : State.t;
  discarded : Card.t list;
}

type choose_direction =
  | Next
  | Prev

let choose_from_hand ({ table = player, _; state; _ } as game) direction =
  let length = List.length player.hand in
  let get_index i =
    let x = match direction with Next -> i + 1 | Prev -> i + length - 1 in
    x mod length
  in
  let choosing =
    match state.choosing with
    | State.Hand i | Discarding i -> State.Hand (get_index i)
  in
  { game with state = { state with choosing } }

let next ({ table = player, opponents; deck; turn; _ } as game) =
  let n = if Player.empty_hand player then 5 else 2 in
  let cards, deck = Deck.draw n deck in
  (* TODO: Reshuffle discarded when [List.length cards <> n] *)
  let player = Player.update_hand player cards in
  {
    game with
    table = (player, opponents);
    turn = turn + 1;
    state = State.start;
    deck;
  }

let current_player { table = player, _; _ } = player

let pass game =
  let Player.{ hand; _ } = current_player game in
  if List.length hand > 7 then
    {
      game with
      state =
        { game.state with choosing = Discarding 0; message = "discarding" };
    }
  else
    next
      {
        game with
        table = Table.turn game.table;
        state = { game.state with message = "" };
      }

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
        cards_played = game.state.cards_played + 1;
        choosing = Hand 0;
        message =
          Printf.sprintf "%s played [%s]." player.name (Card.display card);
      };
  }

let discard game = game

let play game =
  match game.state.choosing with
  | Hand i ->
      if game.state.cards_played = 3 then
        {
          game with
          state =
            {
              game.state with
              message = "Can't play any more cards in this turn.";
            };
        }
      else
        let card, player = Player.remove_from_hand i (current_player game) in
        play_card card { game with table = Table.update player game.table }
  | Discarding i ->
      let player = current_player game in
      if List.length player.hand <= 7 then pass game
      else
        let card, player = Player.remove_from_hand i player in
        {
          game with
          discarded = card :: game.discarded;
          table = Table.update player game.table;
        }

let running { deck; _ } = not (Deck.is_empty deck)

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
  next { table; deck; turn = 0; state = State.start; discarded = [] }
