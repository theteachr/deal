module Hand = struct
  type t = Card.t list [@@deriving show]

  let empty = []
  let is_empty = List.is_empty

  let remove_card index hand =
    let rec remove cards index acc =
      match cards with
      | [] -> failwith "index out of bounds"
      | card :: cards ->
          if index = 0 then (card, List.rev_append acc cards)
          else remove cards (index - 1) (card :: acc)
    in
    remove hand index []
end

module Bank = struct
  type t = Card.Money.t list [@@deriving show]

  let empty = []
  let value cards = cards |> List.map Card.Money.value |> List.fold_left ( + ) 0
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
      (match Properties.find_opt color properties with
      | Some set -> set
      | None -> Card.Property.Set.create ())
      |> Card.Property.Set.add_property card
    in
    { assets with properties = Properties.add color set properties }

  let empty = { bank = Bank.empty; properties = Properties.empty }

  let rent color (properties, buildings) =
    let property_rent =
      (Card.Property.Set.rents color).(List.length properties - 1)
    in
    let buildings_rent =
      buildings |> List.map Card.Action.building_rent |> List.fold_left ( + ) 0
    in
    property_rent + buildings_rent

  let full_property_sets { properties; _ } =
    properties
    |> Properties.bindings
    |> List.map snd
    |> List.filter Card.Property.Set.is_full
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

let remove_from_hand index player =
  let card, hand = Hand.remove_card index player.hand in
  (card, { player with hand })

let has_full_set color player =
  player.assets.properties
  |> Assets.Properties.find_opt color
  |> Option.fold ~none:false ~some:Card.Property.Set.is_full
