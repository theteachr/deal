type t = Card.t list

let default =
  Card.
    [
      (* Property Cards *)
      [
        property Color.Brown "Baltic Avenue";
        property Color.Brown "Mediterranean Avenue";
      ];
      [ property Color.Blue "Broadwalk"; property Color.Blue "Park Place" ];
      [
        property Color.Green "North California Avenue";
        property Color.Green "Pacific Avenue";
        property Color.Green "Pennsylvania Avenue";
      ];
      [
        property Color.Sky_blue "Connecticut Avenue";
        property Color.Sky_blue "Oriental Avenue";
        property Color.Sky_blue "Vermont Avenue";
      ];
      [
        property Color.Orange "New York Avenue";
        property Color.Orange "St. James Place";
        property Color.Orange "Tennessee Avenue";
      ];
      [
        property Color.Magenta "Virginia Avenue";
        property Color.Magenta "St. Charles Place";
        property Color.Magenta "States Avenue";
      ];
      [
        property Color.Black "Short Line";
        property Color.Black "B. & O. Railroad";
        property Color.Black "Reading Railroad";
        property Color.Black "Pennsylvania Railroad";
      ];
      [
        property Color.Red "Kentucky Avenue";
        property Color.Red "Indiana Avenue";
        property Color.Red "Illinois Avenue";
      ];
      [
        property Color.Turquoise "Water Works";
        property Color.Turquoise "Electric Company";
      ];
      [
        property Color.Yellow "Ventnor Avenue";
        property Color.Yellow "Marvin Avenue";
        property Color.Yellow "Atlantic Avenue";
      ];
      (* Wild Cards *)
      [
        Property.dual Color.(Blue, Green) 4;
        Property.dual Color.(Turquoise, Brown) 1;
        Property.dual Color.(Green, Black) 4;
        Property.dual Color.(Sky_blue, Black) 4;
        Property.dual Color.(Turquoise, Black) 2;
      ]
      @ List.init 2 (Fun.const @@ Property.dual Color.(Orange, Magenta) 2)
      @ List.init 2 (Fun.const @@ Property.dual Color.(Yellow, Red) 3)
      @ List.init 2 (Fun.const @@ Property.wild Color.Blue)
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
      List.init 2 (Fun.const @@ rent Color.(Green, Blue) 1);
      List.init 2 (Fun.const @@ rent Color.(Brown, Sky_blue) 1);
      List.init 2 (Fun.const @@ rent Color.(Magenta, Orange) 1);
      List.init 2 (Fun.const @@ rent Color.(Black, Turquoise) 1);
      List.init 2 (Fun.const @@ rent Color.(Red, Yellow) 1);
    ]
  |> List.flatten

(* Taken from https://stackoverflow.com/a/15095713 *)
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
