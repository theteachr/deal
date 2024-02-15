module Hand = struct
  type t = Card.t list

  let empty = []
  let is_empty = List.is_empty
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

  let add_money money assets = { assets with bank = money :: assets.bank }

  let add_property card ({ properties; _ } as assets) =
    let color = Card.Property.color card in
    let set =
      match Properties.find_opt color properties with
      | Some set -> set
      | None ->
          Card.Property.Set.create () |> Card.Property.Set.add_property card
    in
    { assets with properties = Properties.add color set properties }

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

let add_property property player =
  { player with assets = Assets.add_property property player.assets }

let add_money money player =
  { player with assets = Assets.add_money money player.assets }
