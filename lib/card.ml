module Dual = struct
  type colored =
    | Left
    | Right
  [@@deriving show]

  type t = {
    colors : Color.t * Color.t;
    colored : colored option;
  }
  [@@deriving show]

  let of_colors colors = { colors; colored = None }

  let color { colored; colors = a, b } =
    colored |> Option.map (function Left -> a | Right -> b)

  let switch ({ colored; _ } as dual) =
    {
      dual with
      colored = colored |> Option.map (function Left -> Right | Right -> Left);
    }

  let choose choice dual = { dual with colored = Some choice }

  let display { colors = lcolor, rcolor; colored } =
    let open Color in
    let open Printf in
    Option.fold
      ~some:(function
        | Left -> sprintf "[%s]%s" (display lcolor) (display rcolor)
        | Right -> sprintf "%s[%s]" (display lcolor) (display rcolor))
      ~none:(sprintf "%s%s" (display lcolor) (display rcolor))
      colored
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

  let building_rent = function Hotel -> 4 | House -> 3

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
  (* TODO: Rent Cards in the bank
     Rent Cards can also be banked. Decide whether to add a variant here or
     move the [Rent] module inside [Action]. *)
  type t =
    | Money of int
    | Action of Action.t
  [@@deriving show]

  let value = function
    | Money value -> value
    | Action action -> Action.value action

  let display = function
    | Money value -> Printf.sprintf "(%d) Money" value
    | Action action ->
        Printf.sprintf "(%d) %s" (Action.value action) (Action.name action)

  let display_short = function
    | Money value -> Printf.sprintf "(%d)" value
    | Action action ->
        Printf.sprintf "(%d) %s" (Action.value action) (Action.name action)
end

module Property = struct
  type t =
    | Simple of Color.t * string
    | Dual of Dual.t * int
    | Wild of Color.t option
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
    | Dual (dual, _) -> Dual.color dual |> Option.get
    | Wild color -> color |> Option.get

  let display = function
    | Simple (color, name) ->
        let open Color in
        Printf.sprintf "(%d) %s %s" (value color) (display color) name
    | Dual (dual, value) ->
        Printf.sprintf "(%d) %s Wild Property" value (Dual.display dual)
    | Wild None -> Printf.sprintf "(%d) Wild Property" 0
    | Wild (Some color) ->
        Printf.sprintf "(%d) %s Wild Property" 0 (Color.display color)

  module Set = struct
    (* FIXME: Invalid state
       [ ([], [ Action.House ]) ] is an invalid state. We can't have buildings
       on top of incomplete sets. (GADT?) *)

    (* FIXME: Invalid state
       [ ([ Simple (Color.Blue, "PARK PLACE"), Wild Color.Black ], []) ] can't
       happen. *)
    type nonrec t = t list * Action.building list

    let rents = function
      | Color.Black -> [| 1; 2; 3; 4 |]
      | Blue -> [| 3; 8 |]
      | Brown -> [| 1; 2 |]
      | Green -> [| 2; 4; 7 |]
      | Magenta -> [| 1; 2; 4 |]
      | Orange -> [| 1; 3; 5 |]
      | Red -> [| 2; 3; 6 |]
      | Sky_blue -> [| 1; 2; 3 |]
      | Turquoise -> [| 1; 2 |]
      | Yellow -> [| 2; 4; 6 |]

    let create () = ([], [])

    let add_property property (properties, buildings) =
      (property :: properties, buildings)

    let add_building building (properties, buildings) =
      (properties, building :: buildings)

    let is_full (properties, _) =
      match properties with
      | [] -> false
      | property :: _ ->
          List.length properties = Array.length (rents @@ color property)
  end

  let simple color name = Simple (color, name)
  let dual colors value = Dual (Dual.of_colors colors, value)
  let wild = Wild None
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
let rent colors value = Rent (Rent.dual colors value)
let wild_rent = Rent Rent.wild

let display = function
  | Money card -> Printf.sprintf "(%d) Money" (Money.value card)
  | Property card -> Property.display card
  | Action card -> Action.(Printf.sprintf "(%d) %s" (value card) (name card))
  | Rent card -> Rent.display card
