module Hand = struct
  type t = Card.t list

  let empty = []
  let is_empty = List.is_empty
  let to_string hand = hand |> List.map Card.show |> String.concat "\n"
end

module Bank = struct
  type t = Card.Money.t list

  let empty = []
end

module Assets = struct
  module Properties = Map.Make (Color)

  type t = {
    bank : Bank.t;
    properties : Card.Property.Set.t Properties.t;
  }

  let empty = { bank = Bank.empty; properties = Properties.empty }
end

type t = {
  name : string;
  hand : Hand.t;
  assets : Assets.t;
}

let create name = { name; hand = Hand.empty; assets = Assets.empty }

let update_hand player cards =
  { player with hand = List.rev_append cards player.hand }

let empty_hand { hand; _ } = Hand.is_empty hand
