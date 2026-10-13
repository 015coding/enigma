open OUnit2
open Enigma

let index_test name input expected =
  name >:: fun _ ->
    assert_equal expected (index input)

let identify = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rotor_I = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
let rotor_II = "AJDKSIRUXBLHWTMCQGZNPYFVOE"
let rotor_III = "BDFHJLCPRTXVZNYEIWGAKMUSQO"

let reflector_B = "YRUHQSLDPXNGOKMIEBFZCWVJAT"
let reflector_C = "FVPJIAOYEDRZXWGCTKUQSBNMHL"
let map_r_to_l_test name wiring top input expected = 
  name >:: fun _ -> 
    assert_equal expected (map_r_to_l wiring top input)

let map_l_to_r_test name wiring top input expected = 
  name >:: fun _ ->
    assert_equal expected (map_l_to_r wiring top input)

let map_refl_test name wiring input expected = 
  name >:: fun _ ->
    assert_equal expected (map_refl wiring input)

let map_plug_test name plugs c expected =
  name >:: fun _ ->
    assert_equal expected (map_plug plugs c)

let r1 = { wiring = rotor_I; turnover = 'Q' }
let r2 = { wiring = rotor_II; turnover = 'E' }
let r3 = { wiring = rotor_III; turnover = 'V' }   
let simple_config = {
    refl = identify;
    rotors = [];
    plugboard = [];
}
let hard_config = {
  refl = reflector_B;
  rotors = [
    { rotor = r1; top_letter = 'M' };
    { rotor = r2; top_letter = 'E' };
    { rotor = r3; top_letter = 'Z' };
  ];
  plugboard = [
    ('A', 'Z');
    ('G', 'P');
    ('B', 'Q');
    ('C', 'X');
  ];
}

let cipher_char_test name config c expected =
  name >:: fun _ -> 
    assert_equal expected (cipher_char config c)
let cipher_alphabet_test name config expected =
  name >:: fun _ ->
    let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" in
    let actual = String.map (cipher_char config) alphabet in
    assert_equal ~printer:(fun s -> s) expected actual


let make_step_config left middle right =
  {
    refl = reflector_B;
    rotors = [
      {
        rotor = { wiring = rotor_III; turnover = 'V' };
        top_letter = left;
      };
      {
        rotor = { wiring = rotor_II; turnover = 'E' };
        top_letter = middle;
      };
      {
        rotor = { wiring = rotor_I; turnover = 'Q' };
        top_letter = right;
      };
    ];
    plugboard = [];
  }

let step_test name input expected =
  name >:: fun _ ->
    assert_equal expected (step input)


let full_cipher_config = {
  refl = reflector_B;
  rotors = [
    {
      rotor = { wiring = rotor_I; turnover = 'Q' };
      top_letter = 'F';
    };
    {
      rotor = { wiring = rotor_II; turnover = 'E' };
      top_letter = 'U';
    };
    {
      rotor = { wiring = rotor_III; turnover = 'V' };
      top_letter = 'N';
    };
  ];
  plugboard = [('A', 'Z')];
}

let cipher_test name config input expected =
  name >:: fun _ ->
    assert_equal
      ~printer:(fun s -> s)
      expected
      (cipher config input)

let suite =
  "Enigma test suite" >::: [
    index_test "index of A is 0" 'A' 0;
    index_test "index of Z is 25" 'Z' 25;
    index_test "index of B is 1" 'B' 1;
    index_test "index of M is 12" 'M' 12;
    index_test "index of Y is 24" 'Y' 24;


    map_r_to_l_test "identify rotor A" identify 'A' 0 0;
    map_r_to_l_test "identify rotor M" identify 'M' 25 25;
    map_r_to_l_test "rotor_I at A" rotor_I 'A' 0 4;
    map_r_to_l_test "rotor_II at C" rotor_II 'C' 2 16;
    map_r_to_l_test "rotor_III at O" rotor_III 'O' 14 17;

    map_l_to_r_test "identify rotor A" identify 'A' 0 0;
    map_l_to_r_test "identify rotor M" identify 'M' 25 25;
    map_l_to_r_test "rotor_I E to A" rotor_I 'A' 4 0;
    map_l_to_r_test "rotor_I at F" rotor_I 'F' 10 14;
    map_l_to_r_test "rotor I maps A to U at A" rotor_I 'A' 0 20 ;

    map_refl_test "identify reflector A" identify 0 0;
    map_refl_test "reflector_B maps A to Y" reflector_B 0 24;
    map_refl_test "reflector_B maps Y to A" reflector_B 24 0;
    map_refl_test "reflector_C maps A to C" reflector_C 0 5;
    map_refl_test "reflector C maps F to A" reflector_C 5 0;

    map_plug_test "AZ map A to Z" [('A' , 'Z')] 'A' 'Z';
    map_plug_test "BD map D to B" [('B' , 'D')] 'D' 'B';
    map_plug_test "AB map A to B" [('X' , 'Y') ; ('A' , 'B')] 'A' 'B';
    map_plug_test "empty plugboard keeps M" [] 'M' 'M';
    map_plug_test "XY maps Y to X"  [('A', 'Z'); ('X', 'Y')] 'Y' 'X';

    cipher_char_test "identify map A to A"  simple_config 'A' 'A' ;
    cipher_alphabet_test "hard config 1: mixed offsets and plugboard" hard_config "VJXLYWURNBSDQIPOMHKZGAFCET" ;
    cipher_char_test "identity maps Z to Z" simple_config 'Z' 'Z';
    cipher_char_test "reflector B maps A to Y" { simple_config with refl = reflector_B } 'A' 'Y';
    cipher_char_test "plugboard changes reflected A to Z" { simple_config with refl = reflector_B; plugboard = [('A', 'Z')]} 'Y' 'Z' ;


(* Rule 1: Rightmost rotor *)
  step_test "rightmost O to P"
    (make_step_config 'K' 'D' 'O')
    (make_step_config 'K' 'D' 'P');

  step_test "rightmost wraps Z to A"
    (make_step_config 'K' 'D' 'Z')
    (make_step_config 'K' 'D' 'A');

  step_test "right turnover steps middle"
    (make_step_config 'K' 'D' 'Q')
    (make_step_config 'K' 'E' 'R');

  step_test "middle turnover steps left"
    (make_step_config 'K' 'E' 'R')
    (make_step_config 'L' 'F' 'S');

  step_test "both turnovers step once"
    (make_step_config 'K' 'E' 'Q')
    (make_step_config 'L' 'F' 'R');

  step_test "leftmost turnover alone"
    (make_step_config 'V' 'D' 'P')
    (make_step_config 'V' 'D' 'Q');

  cipher_test "empty input returns empty string"
    full_cipher_config "" "";

  cipher_test "identity machine keeps HELLO"
    simple_config "HELLO" "HELLO";

  cipher_test "single character stepping"
    full_cipher_config "Y" "O";

  cipher_test "full Enigma encrypts YNGXQ to OCAML"
    full_cipher_config "YNGXQ" "OCAML";

  cipher_test "full Enigma decrypts OCAML to YNGXQ"
    full_cipher_config "OCAML" "YNGXQ";


 
  ]

let () = run_test_tt_main suite
