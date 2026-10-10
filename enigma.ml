type rotor = {
  wiring : string;
  turnover : char;
}

type oriented_rotor = {
  rotor : rotor;
  top_letter : char;
}

type config = {
  refl : string;
  rotors : oriented_rotor list;
  plugboard : (char * char) list;
}

let index c = Char.code c - Char.code 'A'
(* let map_r_to_l _wiring _top_letter _input_pos =
  failwith "Unimplemented" *)

let map_r_to_l wiring top_letter input_pos = 
  let offset = index top_letter in
  let internal_pos = (input_pos + offset) mod 26 in
  let letter = String.get wiring internal_pos in
  let mapped_pos = index letter in (mapped_pos - offset + 26) mod 26
(* let map_l_to_r _wiring _top_letter _input_pos =
  failwith "Unimplemented" *)

let map_l_to_r wiring top_letter input_pos =
  let offset = index top_letter in
  let internal_pos = (input_pos + offset) mod 26 in
  let letter = Char.chr (internal_pos + Char.code 'A') in
  let mapped_pos = String.index wiring letter in (mapped_pos - offset + 26) mod 26

(* let map_refl _wiring _input_pos =
  failwith "Unimplemented" *)

let map_refl wiring input_pos =
  let letter_index = String.get wiring input_pos in
  Char.code letter_index - Char.code 'A'
(* let map_plug _plugs _c =
  failwith "Unimplemented" *)
let rec map_plug plugs c =
  match plugs with
  | [] -> c
  | (a,b) :: rest -> if c == a then b 
  else if c == b then a else map_plug rest c

(* Helper Function *)
let rec map_rotor_r_to_l rotors pos =
  match rotors with
  | [] -> pos
  | r :: rest -> 
    let next_pos = map_r_to_l r.rotor.wiring r.top_letter pos in map_rotor_r_to_l rest next_pos
let rec map_rotor_l_to_r rotors pos =
  match rotors with
  | [] -> pos
  | r :: rest -> 
    let next_pos = map_l_to_r r.rotor.wiring r.top_letter pos in map_rotor_l_to_r rest next_pos

(* let cipher_char _config _c =
  failwith "Unimplemented" *)
let cipher_char config c =
  let plugged = map_plug config.plugboard c in

  let input_pos = index plugged in
  let forward_pos = map_rotor_r_to_l (List.rev config.rotors) input_pos in
  let reflected_pos = map_refl config.refl forward_pos in
  let reverse_pos = map_rotor_l_to_r config.rotors reflected_pos in
  let letter = Char.chr (reverse_pos + Char.code 'A') in 
  map_plug config.plugboard letter

let step _config =
  failwith "Unimplemented"

let cipher _config _s =
  failwith "Unimplemented"

let hours_worked = 0
