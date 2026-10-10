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


let suite =
  "Enigma test suite" >::: [
    index_test "index of A is 0" 'A' 0;
    index_test "index of Z is 25" 'Z' 25;

    map_r_to_l_test "identify rotor A" identify 'A' 0 0;
    map_r_to_l_test "identify rotor M" identify 'M' 25 25;
    map_r_to_l_test "rotor_I at A" rotor_I 'A' 0 4;
    map_r_to_l_test "rotor_II at C" rotor_II 'C' 2 16;
    map_r_to_l_test "rotor_III at O" rotor_III 'O' 14 17;

    map_l_to_r_test "identify rotor A" identify 'A' 0 0;
    map_l_to_r_test "identify rotor M" identify 'M' 25 25;
    map_l_to_r_test "rotor_I E to A" rotor_I 'A' 4 0;
    map_l_to_r_test "rotor_I at F" rotor_I 'F' 10 14;

    map_refl_test "identify reflector A" identify 0 0;
    map_refl_test "reflector_B maps A to Y" reflector_B 0 24;
    map_refl_test "reflector_B maps Y to A" reflector_B 24 0;
    map_refl_test "reflector_C maps A to C" reflector_C 0 5;

    map_plug_test "AZ map A to Z" [('A' , 'Z')] 'A' 'Z';
    map_plug_test "BD map D to B" [('B' , 'D')] 'D' 'B';
    map_plug_test "AB map A to B" [('X' , 'Y') ; ('A' , 'B')] 'A' 'B';
  ]

let () = run_test_tt_main suite
