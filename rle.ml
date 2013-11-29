open Parser
open Rle_action
open Mcell

(* Prend l'ast du rle pour l'écrire dans une matrice *)
let make_matrix a_list size =
  let mat = Array.make_matrix (1 lsl size) (1 lsl size) false in
  let column = ref 0 in
  let line = ref 0 in

  let fill_matrix nb =
   for i = !column to !column + nb -1 do
     mat.(!line).(i) <- true;
   done
  in

  let step = function
    | On n -> 
      fill_matrix n;
      (*Array.fill mat.(!line) !column (!column + n) true;*)
      column := !column + n
    | Off n -> column := !column + n
    | Newline n -> line := !line + n;
      column := 0
  in

  List.iter step a_list;
  mat

(* Crée un mcell à partir d'un ast créé par le parser *)
let make_mcell a_list size =
  let m = ref (Mcell.empty size) in
  let column = ref 0 in
  let line = ref 0 in

  (* allume n cellules consécutives dans le mcell *)
  let rec step_on n = 
    if n = 0 then ()
    else
      begin
        m := Mcell.set !m !line !column true;
        incr column;
        step_on (n-1)
      end
  in
  
  let rec step = function
    | On n -> step_on n
    | Off n -> column := !column + n
    | Newline n -> line := !line + n;
      column := 0
  in
  List.iter step a_list;
  !m  

(* Lit un fichier .rle et renvoie le mcell correspondant *)
let read file =
  let file = open_in file in
  let lexbuf = Lexing.from_channel file in
  let s, list = Parser.file Lexer.token lexbuf in
  close_in file;
  make_mcell list s

(* Crée la description du .rle correspondant au mcell donnée *)
let make_rle mcell =

  let h = Hashtbl.create 5003 in

  let transform m =
    if m == Mcell.on then On(1)
    else Off(1)
  in

  (* Renvoie simplement la chaine correspondant au Rle_action *)
  let repr a = 
    let repr_int x =
      if x = 1 then "" else string_of_int x
    in
    match a with
        On x -> (repr_int x) ^ "o"
      | Off x -> (repr_int x) ^ "b"
      | Newline x -> (repr_int x) ^ "$"
  in 

  (* Fusionne des actions *)
  let merge_actions a1 a2 =
    match a1, a2 with
      | On x, On y -> Simple (On(x + y))
      | Off x, Off y -> Simple (Off(x + y))
      | Off _, Newline x -> Simple (Newline x)
      | Newline x, Newline y -> Simple (Newline (x + y))
      | _, _ -> Complex (a1, "", a2)
  in

  let merge_action_string a str left =
    (* left indique que l'action est mergée sur la gauche du string, sinon
       sur la droite *)
    let a = repr a in
    if left then a ^ str
    else str ^ a
  in

  (* Fusionne deux actions, dont celle de droite est de type complexe
     -> on fusionne l'autre à sa gauche *)
  let merge_complex_actions_left a1 complex_a1 str complex_a2 =
    let merged = merge_actions a1 complex_a1 in
    match merged with
        Simple a -> Complex (a, str, complex_a2)
      | Complex (l, _, r) -> let str = merge_action_string r str true in
                             Complex (l, str, complex_a2)
  in
  
  (* Inverse de la fonction précédente *)
  let merge_complex_actions_right complex_a1 str complex_a2 a1 =
    let merged = merge_actions complex_a2 a1 in
    match merged with
        Simple a -> Complex (complex_a1, str, a)
      | Complex (l, _, r) -> let str = merge_action_string l str false in
                             Complex (complex_a1, str, r)
  in

  (* Fusionne deux actions situées sur la même ligne *)
  let merge_vertically west east =
    match west, east with
      | Simple a1, Simple a2 -> merge_actions a1 a2
      
      | Simple a1, Complex (ac1, str, ac2) -> 
        merge_complex_actions_left a1 ac1 str ac2
      
      | Complex (ac1, str, ac2), Simple a1 -> 
        merge_complex_actions_right ac1 str ac2 a1
      
      | Complex (ac1, str, ac2), Complex (ac1', str', ac2') ->
        let merged = merge_actions ac2 ac1' in
        let merged_str =
          begin
            match merged with
                Simple a -> str ^ (merge_action_string a str' true)
              | Complex (a1, _, a2) ->  
                (merge_action_string a1 str false) ^ 
                  (merge_action_string a2 str' true)
          end
        in
        Complex(ac1, merged_str, ac2')
  in

  (* Concatène deux listes d'actions de manière verticales *)
  let rec concat l1 l2 =
    assert (List.length l1 = List.length l2);
    match l1, l2 with
      | [], [] -> []
      | a1 :: l1, a2 :: l2 -> (merge_vertically a1 a2) :: (concat l1 l2)
      | _, _ -> assert false
  in
  
  (* Algorithme principal *)
  let rec compute m =
    if Hashtbl.mem h m.id then
      Hashtbl.find h m.id
    else
      let res =
        if m.size = 1 then
          let nw = Simple (transform m.nw) in
          let ne = Simple (transform m.ne) in
          let sw = Simple (transform m.sw) in
          let se = Simple (transform m.se) in
          [merge_vertically nw ne; merge_vertically sw se]
        else
          let nw = compute m.nw in
          let ne = compute m.ne in
          let sw = compute m.sw in
          let se = compute m.se in
          (concat nw ne) @ (concat sw se)
      in
      Hashtbl.add h m.id res;
      res      
  in
  
  let add_newline list elt =
    (merge_vertically elt (Simple (Newline 1))) :: list
  in

  let to_string = function
    | Simple a -> repr a
    | Complex (a1, str, a2) -> (repr a1) ^ str ^ (repr a2)
  in

  let rle = compute mcell in
  let rle = List.fold_left add_newline [] rle in
  let rle = List.rev rle in
  let first = List.hd rle in
  to_string (List.fold_left merge_vertically first (List.tl rle))

let write mcell file =
  let file = open_out file in
  let rle = make_rle mcell in
  let s = 1 lsl mcell.size in
  let header = 
    "# Made with hashlife-ocaml\n" ^
      "x = " ^ (string_of_int s) ^
      ", y = " ^ (string_of_int s) ^
      ", rule = B3/S23\n"
  in
  output_string file header;
  output_string file (rle ^ "!");
  close_out file
      

(*let read_into_matrix file = 
  let file = open_in file in
  let lexbuf = Lexing.from_channel file in
  let s, list = Parser.file Lexer.token lexbuf in
  make_matrix list s*)
