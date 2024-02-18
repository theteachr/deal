module Table = struct
  type t = Player.t * Player.t list

  let turn (current, opponents) =
    match opponents @ [ current ] with
    | current :: opponents -> (current, opponents)
    | _ -> failwith "should not be empty"
end

module State = struct
  type choosing = Hand of int

  type t = {
    cards_played : int;
    choosing : choosing;
  }

  let start = { cards_played = 0; choosing = Hand 0 }
end

type t = {
  table : Table.t;
  deck : Deck.t;
  turn : int;
  state : State.t;
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
    match state.choosing with State.Hand i -> State.Hand (get_index i)
  in
  { game with state = { state with choosing } }

let start_turn { table = player, opponents; deck; turn; _ } =
  let n = if Player.empty_hand player then 5 else 2 in
  let cards, deck = Deck.draw n deck in
  let player = Player.update_hand player cards in
  let table = (player, opponents) in
  { table; deck; turn = turn + 1; state = State.start }

let pass game = { game with table = Table.turn game.table } |> start_turn

let play ({ table = player, opponents; state; _ } as game : t) : t =
  let card, player =
    match state.choosing with Hand i -> Player.remove_from_hand i player
  in
  let player =
    match card with
    | Property card -> Player.add_property card player
    | Money card -> Player.add_money card player
    | Action _ -> failwith "todo: play action card"
    | Rent _ -> failwith "todo: play rent card"
  in
  let table = (player, opponents) in
  if state.cards_played = 2 then
    { game with table = Table.turn table } |> start_turn
  else
    {
      game with
      table;
      state = { state with cards_played = state.cards_played + 1 };
    }

let running { deck; _ } = not (Deck.is_empty deck)

let create players =
  let table =
    match players with
    | current :: opponents -> (current, opponents)
    | [] -> failwith "no players"
  in
  { table; deck = Deck.(shuffle default); turn = 0; state = State.start }
