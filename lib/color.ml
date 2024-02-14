type t =
  | Black
  | Blue
  | Brown
  | Green
  | Magenta
  | Orange
  | Red
  | Sky_blue
  | Turquoise
  | Yellow
[@@deriving ord, show]

let value = function
  | Black -> 2
  | Blue -> 4
  | Brown -> 1
  | Green -> 3
  | Magenta -> 2
  | Orange -> 2
  | Red -> 3
  | Sky_blue -> 1
  | Turquoise -> 2
  | Yellow -> 3
