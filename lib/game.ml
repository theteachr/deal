type t = {
  deck : int Card.t list;
  player : Player.t;
  opponents : Player.t list;
}
[@@deriving show]

let default =
  {
    deck = [];
    player = { name = "theteachr" };
    opponents = [ { name = "procrastination" } ];
  }
[@@deriving show]

let to_string { deck; player; opponents } =
  Printf.
    [
      sprintf "Deck has %d card(s) left" (List.length deck);
      sprintf "%s is the current player" player.name;
      sprintf "%s is the next player" (List.hd opponents).name;
    ]
  |> String.concat "\n"
