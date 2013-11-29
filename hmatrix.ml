
open Mcell

(**
   Convention :
   matrice.(i).(j) où i = ligne et j = colonne
**)

let mcell_to_array mcell =
  (* Convertie la size de mcell (qui correspond à la dimension)
     en une size pour le tableau *)
  let array_size n = 1 lsl n in

  (* On crée la matrice *)
  let mat_size = array_size (Mcell.size mcell) in
  let matrice = Array.make_matrix mat_size mat_size false in
  
  (* Etape pour la fabrication de la matrice *)
  let rec step mcell x y =
    let dim = Mcell.size mcell in
    (* Si c'est une Mcell de taille > 1, on réitère les étapes
       pour tous les corners *)
    if dim > 0 then begin
      if not (Mcell.is_empty mcell ) then begin
	let d = array_size (dim - 1) in (*=la moitié de la taille de la Mcell*)
	step mcell.nw x y;
	step mcell.ne (x + d) y;
	step mcell.sw x (y + d);
	step mcell.se (x + d) (y + d)
      end
    end
      (* Sinon on affecte la matrice en fonction
	 de l'état de la cellule de taille 1 *)
    else
      if mcell == Mcell.on then matrice.(y).(x) <- true
  in
  step mcell 0 0;
  matrice


let rec log2 n = if n = 0 then -1 else 1 + log2 (n / 2)

(* une version un peu plus efficace de log2
let tab_log2 = Array.init 256 log2
let log2_8 n = tab_log2.(n)

let log2_16 n = let h = n lsr 8 in if h = 0 then log2_8 n else 8 + log2_8 h
let log2_32 n = let h = n lsr 16 in if h = 0 then log2_16 n else 16 + log2_16 h
let log2_64 n = let h = n lsr 32 in if h = 0 then log2_32 n else 32 + log2_32 h

let () =
  for i = 0 to 61 do assert (let n = 1 lsl i in log2 n = log2_64 n) done
*)

(**
   array_to_mcell:
   Créé un Mcell à partir d'une matrice de booléen
   
   array array bool -> Mcell
**)
let array_to_mcell array =
  let n = Array.length array in
  let size = log2 n in
  assert (n = 1 lsl size && Array.length array.(0) = n);
  let rec build x y dim =
    (* Verifie d'abord pour une matrice 2*2 *)
    if dim = 0 then
      if array.(x).(y) then Mcell.on else Mcell.off
    else
      let dim = dim - 1 in
      let d = 1 lsl dim in
      let nw = build x       y       dim in
      let ne = build x       (y + d) dim in
      let sw = build (x + d) y       dim in
      let se = build (x + d) (y + d) dim in
      Mcell.create ~nw ~ne ~sw ~se
  in
  build 0 0 size
