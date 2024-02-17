module Dual = struct
  type chosen =
    | Left
    | Right
  [@@deriving show]

  type t = {
    colors : Color.t * Color.t;
    used : chosen;
  }
  [@@deriving show]

  let of_colors colors = { colors; used = Left }

  let display { colors = lcolor, rcolor; used } =
    match used with
    | Left ->
        Printf.sprintf "(%s) %s" (Color.display lcolor) (Color.display rcolor)
    | Right ->
        Printf.sprintf "%s (%s)" (Color.display lcolor) (Color.display rcolor)
end

module Action = struct
  type building =
    | Hotel
    | House
  [@@deriving show]

  type t =
    | Birthday
    | Debt_collector
    | Deal_breaker
    | Double_the_rent
    | Forced_deal
    | Building of building
    | Just_say_no
    | Pass_go
    | Sly_deal
  [@@deriving show]

  let value = function
    | Birthday -> 2
    | Debt_collector -> 3
    | Deal_breaker -> 5
    | Double_the_rent -> 1
    | Forced_deal -> 3
    | Building House -> 3
    | Building Hotel -> 4
    | Just_say_no -> 4
    | Pass_go -> 1
    | Sly_deal -> 3

  let name = function
    | Birthday -> "BIRTHDAY"
    | Debt_collector -> "DEBT COLLECTOR"
    | Deal_breaker -> "DEAL BREAKER"
    | Double_the_rent -> "DOUBLE THE RENT"
    | Forced_deal -> "FORCED DEAL"
    | Building House -> "HOUSE"
    | Building Hotel -> "HOTEL"
    | Just_say_no -> "JUST SAY NO"
    | Pass_go -> "PASS GO"
    | Sly_deal -> "SLY DEAL"
end

module Money = struct
  type t =
    | Money of int
    | Action of Action.t
  [@@deriving show]

  let value = function
    | Money value -> value
    | Action action -> Action.value action
end

module Property = struct
  type t =
    | Simple of Color.t * string
    | Dual of Dual.t * int
    | Wild of Color.t
  [@@deriving show]

  let value = function
    | Simple (color, _) -> Color.value color
    | Dual (_, value) -> value
    | Wild _ -> 0

  let name = function
    | Simple (_, name) -> name
    | Dual _ | Wild _ -> "Wild Card"

  let color = function
    | Simple (color, _) -> color
    | Dual ({ colors = lcolor, rcolor; used }, _) -> (
        match used with Left -> lcolor | Right -> rcolor)
    | Wild color -> color

  module Set = struct
    type nonrec t = t list * Action.building list

    let create () = ([], [])

    let add_property property (properties, buildings) =
      (property :: properties, buildings)

    let add_building building (properties, buildings) =
      (properties, building :: buildings)
  end

  let simple color name = Simple (color, name)
  let dual colors value = Dual (Dual.of_colors colors, value)
  let wild color = Wild color
end

module Rent = struct
  type colors =
    | Dual of Dual.t
    | Wild
  [@@deriving show]

  type t = {
    value : int;
    colors : colors;
  }
  [@@deriving show]

  let value { value; _ } = value
  let dual colors value = { value; colors = Dual (Dual.of_colors colors) }
  let wild = { value = 3; colors = Wild }

  let display { value; colors } =
    let suffix =
      match colors with
      | Dual { colors = a, b; _ } ->
          Printf.sprintf "%s%s Rent" (Color.display a) (Color.display b)
      | Wild -> "Wild Rent"
    in
    Printf.sprintf "(%d) %s" value suffix
end

type t =
  | Money of Money.t
  | Property of Property.t
  | Action of Action.t
  | Rent of Rent.t
[@@deriving show]

let property color name = Property (Property.simple color name)
let money value = Money (Money.Money value)
let action a = Action a
let rent colors (value : int) = Rent (Rent.dual colors value)
let wild_rent = Rent Rent.wild

let display = function
  | Money card -> Printf.sprintf "%d M" (Money.value card)
  | Property card ->
      let color = Property.color card in
      Printf.sprintf "(%d) %s %s" (Color.value color) (Color.display color)
        (Property.name card)
  | Action card -> Action.(Printf.sprintf "(%d) %s" (value card) (name card))
  | Rent card -> Printf.sprintf "%d R" (Rent.value card)
