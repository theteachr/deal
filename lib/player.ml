module Hand = struct
  type t = (Card.Any.t, Card.inactive) Card.t list

  let is_empty = List.is_empty

  let to_string (hand : t) =
    hand
    |> List.map (fun Card.{ value; kind } ->
           Printf.sprintf "{ value = %d; kind = %s }" value (Card.Any.show kind))
    |> String.concat "\n"
end

module Bank = struct
  type t = {
    monies : (Card.Money.t, Card.inactive) Card.t list;
    actions : (Card.Action.t, Card.inactive) Card.t list;
  }

  let empty = { monies = []; actions = [] }
end

module Assets = struct
  module Properties = Map.Make (Color)

  type t = {
    bank : Bank.t;
    properties : Card.Property.t Properties.t;
  }

  let empty = { bank = Bank.empty; properties = Properties.empty }
end

type t = {
  name : string;
  hand : Hand.t;
  assets : Assets.t;
}

let create name = { name; hand = []; assets = Assets.empty }

let update_hand player cards =
  { player with hand = List.rev_append cards player.hand }

let empty_hand { hand; _ } = Hand.is_empty hand
