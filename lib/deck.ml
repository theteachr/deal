type t = Card.t list

let default =
  Card.
    [
      (* Property Cards *)
      [ "Baltic Avenue"; "Mediterranean Avenue" ] |> List.map (property Brown);
      [ "Broadwalk"; "Park Place" ] |> List.map (property Blue);
      [ "North California Avenue"; "Pacific Avenue"; "Pennsylvania Avenue" ]
      |> List.map (property Green);
      [ "Connecticut Avenue"; "Oriental Avenue"; "Vermont Avenue" ]
      |> List.map (property Sky_blue);
      [ "New York Avenue"; "St. James Place"; "Tennessee Avenue" ]
      |> List.map (property Orange);
      [ "Virginia Avenue"; "St. Charles Place"; "States Avenue" ]
      |> List.map (property Magenta);
      [
        "Short Line";
        "B. & O. Railroad";
        "Reading Railroad";
        "Pennsylvania Railroad";
      ]
      |> List.map (property Black);
      [ "Kentucky Avenue"; "Indiana Avenue"; "Illinois Avenue" ]
      |> List.map (property Red);
      [ "Water Works"; "Electric Company" ] |> List.map (property Turquoise);
      [ "Ventnor Avenue"; "Marvin Avenue"; "Atlantic Avenue" ]
      |> List.map (property Yellow);
      (* Wild Cards *)
      Property.
        [
          dual (Blue, Green) 4;
          dual (Turquoise, Brown) 1;
          dual (Green, Black) 4;
          dual (Sky_blue, Black) 4;
          dual (Turquoise, Black) 2;
        ]
      @ List.init 2 (Fun.const @@ Property.dual (Orange, Magenta) 2)
      @ List.init 2 (Fun.const @@ Property.dual (Yellow, Red) 3)
      @ List.init 2 (Fun.const @@ Property.wild Blue)
      |> List.map (fun card -> Property card);
      (* Money Cards *)
      List.init 2 (Fun.const @@ money 5);
      List.init 3 (Fun.const @@ money 4);
      List.init 3 (Fun.const @@ money 3);
      List.init 5 (Fun.const @@ money 2);
      List.init 6 (Fun.const @@ money 1);
      [ money 10 ];
      (* Action Cards *)
      List.init 2 (Fun.const @@ action Deal_breaker);
      List.init 3 (Fun.const @@ action Just_say_no);
      List.init 3 (Fun.const @@ action Sly_deal);
      List.init 4 (Fun.const @@ action Forced_deal);
      List.init 3 (Fun.const @@ action Debt_collector);
      List.init 3 (Fun.const @@ action Birthday);
      List.init 3 (Fun.const @@ action (Building House));
      List.init 3 (Fun.const @@ action (Building Hotel));
      List.init 2 (Fun.const @@ action Double_the_rent);
      List.init 10 (Fun.const @@ action Pass_go);
      (* Rent Cards *)
      List.init 3 (Fun.const @@ wild_rent);
      List.init 2 (Fun.const @@ rent (Green, Blue) 1);
      List.init 2 (Fun.const @@ rent (Brown, Sky_blue) 1);
      List.init 2 (Fun.const @@ rent (Magenta, Orange) 1);
      List.init 2 (Fun.const @@ rent (Black, Turquoise) 1);
      List.init 2 (Fun.const @@ rent (Red, Yellow) 1);
    ]
  |> List.flatten

(* Taken from https://stackoverflow.com/a/15095713. *)
let shuffle deck =
  Random.self_init ();
  deck
  |> List.map (fun card -> (Random.bits (), card))
  |> List.sort compare
  |> List.map snd

let draw n deck =
  let rec pop n drawn = function
    | deck when n = 0 -> (drawn, deck)
    | [] as empty -> (drawn, empty)
    | card :: deck -> pop (n - 1) (card :: drawn) deck
  in
  pop n [] deck

let count = List.length
let is_empty = List.is_empty
