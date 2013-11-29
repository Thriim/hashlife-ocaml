open Graphics
open Mcell
open Hmatrix
open Rle

(** Matrice de Test **)
let mat_test_01 = [|	[| false ; true ; false ; false |];
			[| false ; false ; true ; false |];
			[| true ; true ; true ; false |];
			[| false ; false ; false ; false |] |]
let mat_test_02 = [|	[| false ; false ; false |];
			[| false ; true ; true |];
			[| true ; false ; false |] |]
let mat_test_03 = [|    [| false ; false ; false ; false |];
			[| false ; false ; false ; false |];
			[| true ; true ; false ; false |];
			[| true ; true ; false ; false |] |]

(**
   dessine :
   Fonction d'affichage pour une Mcell sous forme matricielle
   Ouvre une fenêtre et affiche graphiquement la Mcell
   
   array array bool -> unit
**)

let dessine_naif mat_cell =
  (* Paramètre d'affichage *)
  let padding = 50 in

  (* On ouvre une fenetre *)
  open_graph " 800x800";
  set_window_title "Hashlife-ocaml by Couderc-Maison";

  (* Affichage statique de la matrice *)
  let size = Array.length mat_cell in
  let zoom = (size_x () - padding) / size in
  let origin_x = (size_x () / 2 - (size*zoom) / 2) in
  let origin_y = (size_y () / 2 - (size*zoom) / 2) in
  (* On affiche la bordure centrée dans la fenêtre *)
  draw_rect origin_x origin_y (size*zoom) (size*zoom);
  (* Puis on parcours la matrice pour afficher les cellules *)
  let _ = Array.fold_left (fun y ligne ->
    let _ = Array.fold_left (fun x cell ->
      if cell then
	fill_rect (origin_x+x*zoom) (size_y ()-origin_y-(y+1)*zoom) zoom zoom;
      x + 1
    ) 0 ligne
    in
    y + 1 
  ) 0 mat_cell
  in

  (* Boucle qui est réitérée jusqu'à la fin de l'affichage *)
  let rec boucle () =
    
    let event = wait_next_event [Key_pressed] in
    let code = int_of_char event.key in
    
    (* Tant qu'on a pas pressé la touche Echap, on boucle *)
    if not ( code = 27 ) then
      begin
	Printf.eprintf "key = %d\n" code;
	boucle ()
      end
  in
  
  (* On boucle jusqu'à recevoir un signal de sortie géré par la boucle *)
  boucle ();
  
  (* Et on ferme la fenêtre *)
  close_graph ()















let max_scale = 512
let debug = ref false  
    
(* Type représentant les points cardinaux *)
type pc = N | E | W | S

(* Le traitement à faire s'il n'y a pas de découpage dans dessine *)
let rec traitement_dessine ~x ~y ~w ~h ~m ~i ~j ~scale =
  if !debug then Printf.eprintf "size : %d\n" (Mcell.size m);
  if (float w) *. scale <= 1. && not (Mcell.is_empty m) then
    plot (truncate x) (truncate y)
  else if Mcell.is_full m then
    begin
      if !debug then Printf.eprintf "dessine : (%f,%f)\n" x y;
      fill_rect 
	(truncate x) 
	(truncate y) 
	(truncate ((float w) *. scale)) 
	(truncate ((float h) *. scale))
    end
  else if not (Mcell.is_empty m) then
    (if !debug then 
	Printf.eprintf "(%d-%d,%d-%d) not empty/full\n"
	  i (i+w) j (j+h);
    dessine_step ~x ~y ~w ~h ~m ~i ~j ~scale)

and dessine_step ~x ~y ~w ~h ~m ~i ~j ~scale =
  assert (i >= 0);
  assert (j >= 0);
  assert (x >= 0.);
  assert (y >= 0.);
  assert (w >= 0);
  assert (h >= 0);

  if !debug then Printf.eprintf "x=%f y=%f i=%d j=%d w=%d h=%d\n" x y i j w h;

  (* La moitié de la taille de la mcell *)
  let mid = (1 lsl (m.size - 1)) in

  (* On va calculer où se trouve les coins dans notre mcell *)
  let i_at_left = i < mid in
  let j_at_bottom = j < mid in
  let i2_at_left = (i + w) < mid in
  let j2_at_bottom = (j + h) < mid in

  (* Si tout est dans la meme mcell alors on fait le traitement
  avec la mcell en question *)
  if ( i_at_left = i2_at_left && j_at_bottom = j2_at_bottom ) then
    let m,i,j = match (j_at_bottom, i_at_left) with
      | (false,true) -> m.nw,  i,     j-mid 
      | (false,false) -> m.ne, i-mid, j-mid 
      | (true,true) -> m.sw,   i,     j
      | (true,false) -> m.se,  i-mid, j
    in
    if !debug then Printf.eprintf "DECOUPE : aucune\n";
    traitement_dessine x y w h m i j scale
  (* Sinon on découpe et on appelle récursivement avec la meme cell m *)
  else
    (* Si on doit découper que horizontalement *)
    if i_at_left = i2_at_left then
      begin
	if !debug then Printf.eprintf "DECOUPE : h\n";
	let mcell_n = if i_at_left then m.nw else m.ne in
	let mcell_s = if i_at_left then m.sw else m.se in
	let i = if i_at_left then i else i-mid in
	let h1 = mid - j in
	let y2 = y +. (float h1) *. scale in
	let h2 = h - h1 in
	if !debug then Printf.eprintf "verif: i=%d j=%d\n" i j;
	traitement_dessine x y  w h1 mcell_s i j scale;
	traitement_dessine x y2 w h2 mcell_n i 0 scale
      end
    (* Si on doit découper que verticalement *)
    else if j_at_bottom = j2_at_bottom then
      begin
	if !debug then Printf.eprintf "DECOUPE : v\n";
	let mcell_e = if j_at_bottom then m.se else m.ne in
	let mcell_w = if j_at_bottom then m.sw else m.nw in
	let j = if j_at_bottom then j else j-mid in
	let w1 = mid - i in
	let x2 = x +. (float w1) *. scale in
      	let w2 = w - w1 in
	traitement_dessine x  y w1 h mcell_w i j scale;
	traitement_dessine x2 y w2 h mcell_e 0 j scale
      end
    (* Si on doit découper horizontalement et verticalement *)
    else
      begin
	if !debug then Printf.eprintf "DECOUPE : h+v\n";
	let w1 = mid - i in
	let h1 = mid - j in
	let x2 = x +. (float w1) *. scale in
	let y2 = y +. (float h1) *. scale in
	let w2 = w - w1 in
	let h2 = h - h1 in
	traitement_dessine x  y  w1 h1 m.sw i j scale;
	traitement_dessine x2 y  w2 h1 m.se 0 j scale;
	traitement_dessine x  y2 w1 h2 m.nw i 0 scale;
	traitement_dessine x2 y2 w2 h2 m.ne 0 0 scale
      end

let dessine ~i ~j ~w ~h ~m ~scale =
  (* Si le i ou le j est complètement en dehors, on affiche rien *)
  if not ( i >= (1 lsl m.size) || j >= (1 lsl m.size) ) then 
    begin
      let size = 1 lsl m.size in
      let x,w,i = if i < 0 then (float (-i)) *. scale, w+i, 0 else 0.,w,i in
      let y,h,j = if j < 0 then (float (-j)) *. scale, h+j, 0 else 0.,h,j in
      let w = min w (size - i) in
      let h = min h (size - j) in
      dessine_step ~x ~y ~w ~h ~m ~i ~j ~scale
    end

let lancer_anim ?(t=1) m =
  (* On ouvre une fenetre *)
  open_graph " 800x800";
  set_window_title "Hashlife-ocaml by Couderc-Maison";
  auto_synchronize false;
  display_mode false;

  let auto_anime = ref false in
 
  (* Boucle qui est réitérée jusqu'à la fin de l'affichage *)
  let rec boucle m i j t scale =
    if !debug then Printf.eprintf "---------------------------------\n";
    clear_graph ();
    let w = max 1 (truncate (800. /. scale)) in
    dessine ~w ~h:w ~m ~i ~j ~scale;
    synchronize ();
    
    let rec wait_event () =
      if ( key_pressed () ) then
	begin
	  let code = int_of_char (read_key ()) in
	  let move = if scale < 1. then truncate (1. /. scale *. 4.)
	    else 2
	  in
       
	  if code = 122 then (* z *)
	    boucle m i (j+move) t scale
	  else if code = 113 then (* q *)
	    boucle m (i-move) j t scale
	  else if code = 115 then (* s *)
	    boucle m i (j-move) t scale
	  else if code = 100 then (* d *)
	    boucle m (i+move) j t scale
	  else if code = 112 then (* p *)
	    boucle m (i +w/4) (j +w/4) t (scale *. 2.)
	  else if (code = 109) then (* m *)
	    boucle m (i -w/2) (j -w/2) t (scale /. 2.)
	  else if (code = 97) then
	    begin
	      auto_anime := not !auto_anime;
	      boucle m i j t scale
	    end
	  else if (code = 43) then 	(* + *)
            (* "max t (t*2)" ne marche pas ici, même si t*2 devient negatif *)
            let t = if t*2 < 0 then t else t * 2 in
	    boucle m i j t scale
	  else if (code = 45) then 	(* - *)
	    let t = max 1 (t/2) in
	    boucle m i j t scale
	(* Tant qu'on a pas pressé la touche Echap, on boucle *)
	  else if not ( code = 27 ) then
	    begin
	      let i, j, m =
		let size_avant = 1 lsl m.size in
		let m = Mcell.iter ~t m in
		let size_apres = 1 lsl m.size in
		let dec = (size_apres - size_avant) / 2 in  
		i+dec, j+dec, m
	      in
	      boucle m i j t scale
	    end
	end
      else
	if !auto_anime then
          let i,j,m =
	    let size_avant = 1 lsl m.size in
	    let m = Mcell.iter ~t m in
	    let size_apres = 1 lsl m.size in
	    let dec = (size_apres - size_avant) / 2 in  
	    i+dec, j+dec, m
	  in
	  boucle m i j t scale
	else wait_event ()
    in
    wait_event ()
  in
  
  (* On boucle jusqu'à recevoir un signal de sortie géré par la boucle *)
  boucle m 0 0 1 4.;

  (* Et on ferme la fenêtre *)
  close_graph ()



(* Point d'entrée du programme via graphics *)
let dir = "files"^(Filename.dir_sep)
let ext = ".rle"

let () =
  let file = Sys.argv.((Array.length Sys.argv) - 1) in

  if (Sys.argv.(1) = "--test") then Test.lancer_test ()
  else
    (* Si l'argument est un raccourci, on ajoute le chemin pra default *)
    let filename = if not (Filename.check_suffix file ".rle")
      then dir^file^ext else file in
    let mcell = Rle.read filename in

    lancer_anim mcell
