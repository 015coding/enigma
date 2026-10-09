open OUnit2
open Enigma

let index_test name input expected =
  name >:: fun _ ->
    assert_equal expected (index input)

let identify = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rotor_I = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
let rotor_II = "AJDKSIRUXBLHWTMCQGZNPYFVOE"
let rotor_III = "BDFHJLCPRTXVZNYEIWGAKMUSQO"

let map_r_to_l_test name wiring top input expected = 
  name >:: fun _ -> 
    assert_equal expected (map_r_to_l wiring top input)
    
let suite =
  "Enigma test suite" >::: [
    index_test "index of A is 0" 'A' 0;
    index_test "index of Z is 25" 'Z' 25;

    map_r_to_l_test "identify rotor A" identify 'A' 0 0;
    map_r_to_l_test "identify rotor M" identify 'M' 25 25;
    map_r_to_l_test "rotor_I at A" rotor_I 'A' 0 4;
    map_r_to_l_test "rotor_II at C" rotor_II 'C' 2 16;
    map_r_to_l_test "rotor_III at O" rotor_III 'O' 14 17;

  ]

let () = run_test_tt_main suite
