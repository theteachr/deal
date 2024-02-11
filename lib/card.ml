type inactive

type ('a, 'state) t = {
  value : int;
  kind : 'a;
}
[@@deriving show]

module Color_values : sig
  val get : Color.t -> int
end = struct
  module Values = Map.Make (Color)

  let values =
    Values.(
      empty
      |> add Color.Brown 1
      |> add Color.Black 2
      |> add Color.Blue 4
      |> add Color.Sky_blue 1
      |> add Color.Orange 2
      |> add Color.Green 4
      |> add Color.Red 3
      |> add Color.Magenta 2
      |> add Color.Turquoise 2
      |> add Color.Yellow 3)

  let get color = Values.find color values
end

module Property = struct
  type t = {
    name : string;
    color : Color.t;
  }
  [@@deriving show]

  module Rent_vectors = Map.Make (Color)

  let rent_vector (color : Color.t) : int list =
    let rent_vectors = [] |> Rent_vectors.of_list in
    Rent_vectors.find color rent_vectors
end

module Action = struct
  type t
end

module Money = struct
  type t = unit [@@deriving show]
end

module WildProperty = struct
  type t =
    | Dual of Color.t * Color.t
    | Wild
  [@@deriving show]
end

module Any = struct
  type t =
    | Money of Money.t
    | WildProperty of WildProperty.t
    | Property of Property.t
  [@@deriving show]
end

let money value = { value; kind = Any.Money () }

let property color name =
  { value = Color_values.get color; kind = Any.Property { color; name } }
