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

let all =
  [
    Black; Blue; Brown; Green; Magenta; Orange; Red; Sky_blue; Turquoise; Yellow;
  ]

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

let rgb = function
  | Black -> (0, 0, 0)
  | Blue -> (10, 147, 150)
  | Brown -> (150, 75, 0)
  | Green -> (83, 221, 108)
  | Magenta -> (214, 122, 177)
  | Orange -> (255, 120, 79)
  | Red -> (232, 49, 81)
  | Sky_blue -> (132, 218, 235)
  | Turquoise -> (148, 210, 189)
  | Yellow -> (244, 233, 0)

let display color =
  let r, g, b = rgb color in
  Printf.sprintf "\x1b[38;2;%d;%d;%dm●\x1b[0m" r g b

let green_text = Printf.sprintf "\x1b[1;32m%s\x1b[0m"
