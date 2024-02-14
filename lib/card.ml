module Action = struct
  type building =
    | Hotel
    | House
  [@@deriving show]

  type t =
    | Birthday
    | Debt_collector
    | Double_the_rent
    | Forced_deal
    | Building of building
    | Just_say_no
    | Pass_go
    | Sly_deal
  [@@deriving show]
end

module Money = struct
  type t =
    | Money of int
    | Action of Action.t
  [@@deriving show]
end

module Property = struct
  type t =
    | Simple of Color.t * string
    | Dual of Color.t * Color.t
    | Wild
  [@@deriving show]

  module Set = struct
    type nonrec t = t list * Action.building list
  end
end

type t =
  | Money of Money.t
  | Property of Property.t
  | Action of Action.t
[@@deriving show]

let property color name = Property (Property.Simple (color, name))
let money value = Money (Money.Money value)
