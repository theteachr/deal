module Table = struct
  type t = Player.t * Player.t list

  let turn (current, opponents) =
    match opponents @ [ current ] with
    | current :: opponents -> (current, opponents)
    | _ -> failwith "should not be empty"
end

type state = Choosing_from_hand of int

type t = {
  table : Table.t;
  deck : Deck.t;
  turn : int;
  state : state;
}

let choose_prev ({ table = player, _; state; _ } as game) =
  let state =
    match state with
    | Choosing_from_hand i ->
        let length = List.length player.hand in
        Choosing_from_hand ((i + length - 1) mod length)
  in
  { game with state }

let choose_next ({ table = player, _; state; _ } as game) =
  let state =
    match state with
    | Choosing_from_hand i ->
        Choosing_from_hand ((i + 1) mod List.length player.hand)
  in
  { game with state }

let start_turn { table = player, opponents; deck; turn; _ } =
  let n = if Player.empty_hand player then 5 else 2 in
  let cards, deck = Deck.draw n deck in
  let player = Player.update_hand player cards in
  let table = (player, opponents) in
  { table; deck; turn = turn + 1; state = Choosing_from_hand 0 }

let end_turn ({ table; _ } as game : t) : t =
  { game with table = Table.turn table } |> start_turn

let running { deck; _ } = not (Deck.is_empty deck)

let create players =
  let table =
    match players with
    | current :: opponents -> (current, opponents)
    | [] -> failwith "no players"
  in
  { table; deck = Deck.default; turn = 0; state = Choosing_from_hand 0 }
