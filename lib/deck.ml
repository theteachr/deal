type t = Card.t list

let default =
  [
    (* Property Cards *)
    [
      Card.property Color.Brown "Baltic Avenue";
      Card.property Color.Brown "Mediterranean Avenue";
    ];
    (* Money Cards *)
    List.init 10 (Fun.const (Card.money 1));
    [ Card.money 10 ];
  ]
  |> List.flatten

let draw (n : int) (deck : t) : Card.t list * t =
  let rec draw_n n drawn = function
    | deck when n = 0 -> (drawn, deck)
    | [] as empty -> (drawn, empty)
    | card :: deck -> draw_n (n - 1) (card :: drawn) deck
  in
  draw_n n [] deck

let count = List.length
let is_empty = List.is_empty
