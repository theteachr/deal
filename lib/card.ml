type 'a t = { value : int; kind : 'a } [@@deriving show]

module Property = struct
  type t = { color : Color.t } [@@deriving show]
end

module Rent_vectors = Map.Make (Color)

let rent_vector (color : Color.t) : int list =
  let rent_vectors = [] |> Rent_vectors.of_list in
  Rent_vectors.find color rent_vectors
