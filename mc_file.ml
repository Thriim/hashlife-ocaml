open Rle
open Rle_action
open Mcell

let debug = ref false

(* Crée le marco cell décrit par un fichier .mc, après parsing *)
let compute_mcell desc_list =
  let mcell_nb = ref 1 in
  let array_size = List.length desc_list in
  let array = Array.make array_size Mcell.off in

  let get_mcell size nb =
    if nb = 0 then (if !debug then print_endline "Empty !"; empty (size -1))
    else array.(nb - 1)
  in
  
  let create_node size nw ne sw se =
    if !debug then Printf.printf "Cell number = %d\n" !mcell_nb;
    let nw = get_mcell size nw in
    let ne = get_mcell size ne in
    let sw = get_mcell size sw in
    let se = get_mcell size se in
    if !debug then Printf.printf "%d %d %d %d\n" nw.size ne.size sw.size se.size;
    let mcell = 
      create ~nw ~ne ~sw ~se
    in
    array.(!mcell_nb - 1) <- mcell;
    incr mcell_nb
  in

  let create_leaf a_list =
    if !debug then Printf.printf "Cell number = %d\n" !mcell_nb;
    let mcell = make_mcell a_list 3 in
    array.(!mcell_nb - 1) <- mcell;
    incr mcell_nb
  in
  
  let compute_list = function
    | Node (size, nw, ne, sw, se) -> create_node size nw ne sw se
    | Leaf a_list -> create_leaf a_list 
  in

  List.iter compute_list desc_list;
  (* la dernière mcell créée est toujours le résultat *)
  let mc = array.(array_size - 1) in
  Printf.printf "%d %b" mc.size (is_empty mc);
  mc


let print_desc desc_list =
  let print_leaf a_list =
    let act = function
      | On _ -> "*"
      | Off _ -> "."
      | Newline _ -> "$"
    in
    print_endline (List.fold_left (fun str leaf -> str ^ (act leaf)) "" a_list)
  in
  let print_node s nw ne sw se =
    Printf.printf "%d %d %d %d %d\n" s nw ne sw se
  in
  List.iter (
    fun d -> match d with
      | Node (s, nw, ne, sw, se) -> print_node s nw ne sw se
      | Leaf a_list -> print_leaf a_list
  ) desc_list

let read file =
  let file = open_in file in
  let lexbuf = Lexing.from_channel file in
  let desc_list = Mc_parser.file Mc_lexer.token lexbuf in
  if !debug then print_desc desc_list;
  close_in file;
  compute_mcell desc_list


module T = struct
  type t = Mcell.t
  let equal m1 m2 =
    m1.nw == m2.nw && m1.ne == m2.ne && m1.sw == m2.sw && m1.se == m2.se
      
  let hash m = abs (19 * (19 * (19 * m.nw.id + m.ne.id) + m.sw.id) + m.se.id)
end

module H = Hashtbl.Make(T)

let make_mc_file mcell =

  let h = H.create 19 in

  let repr c = 
    if c = "o" then '*'
    else if c = "b" then '.'
    else '$'
  in
  
  let i = ref 1 in
  let nb = ref "" in
  let result = Buffer.create 17 in
  let mc_file = Buffer.create 17 in

  let change_chars c =
    if Char.code c >= 48 && Char.code c <= 57 then
      nb := !nb ^ (Char.escaped c)
    else
      if String.length !nb > 0 then
        begin
          if !debug then print_endline !nb;
          for i = 0 to (int_of_string !nb) - 1 do
            Buffer.add_char result (repr (Char.escaped c))
          done;
          nb := "";
        end
      else
        Buffer.add_char result (repr (Char.escaped c))
  in

  let rec step m =
    try
      H.find h m
    with Not_found ->
      if Mcell.is_empty m then 0 
      else if m.size = 3 then
        begin
          Buffer.clear result;
          nb := "";
          let rle = make_rle m in
          String.iter change_chars rle;
          Buffer.add_char result '\n';
          Buffer.add_string mc_file (Buffer.contents result);
          H.add h m !i;
          incr i;
          !i - 1
        end
      else 
        let nw =
          try
            H.find h m.nw
          with
              Not_found -> step m.nw
        in
        let ne =
          try
            H.find h m.ne
          with
              Not_found -> step m.ne
        in
        let sw =
          try
            H.find h m.sw
          with
              Not_found -> step m.sw
        in
        let se =
          try
            H.find h m.se
          with
              Not_found -> step m.se
        in
        H.add h m !i; 
        Buffer.add_string mc_file
          (Format.sprintf "%d %d %d %d %d\n" m.size nw ne sw se);
        incr i;
        !i - 1
  in
  let _ = step mcell in
  let mc =
    Buffer.contents mc_file
  in
  (* print_endline mc; *)
  mc

let write mcell file =
  let file = open_out file in
  let rec extend_to_3 mcell = 
    if mcell.size < 3 then extend_to_3 (extend mcell)
    else mcell
  in
  let mc = make_mc_file (extend_to_3 mcell) in
  (* print_endline ("Enregistrement : \n" ^ mc); *)
  let header = "# Made with hashlife-ocaml\n" in
  output_string file (header ^ mc);
  close_out file
