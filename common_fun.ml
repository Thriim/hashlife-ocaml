(**
   - VIRER LES REFERENCES POUR LES METTRE DANS UN DES TYPES
   - OPTIMISER LES TYPES
   - FAIRE UN VRAI DRAW_POINT
**)

open Mcop_ast
open Mcell

module M = Map.Make(String)

type window =
    { mutable background : [`Any] Cairo.surface option ;
      mutable zoom_sensibility : float }
type mouse =
    { mutable mx:float ;
      mutable my:float ;
      mutable button_clicked:bool;
      mutable dragged:bool }
type widgets =
    { mutable label_i : GMisc.label;
      mutable label_j : GMisc.label;
      mutable label_scale : GMisc.label;
      mutable label_t : GMisc.label;
      mutable label_pop : GMisc.label;
      mutable button_pause : GButton.button;
      mutable button_zoom : GButton.toggle_button;
      mutable button_edit : GButton.button;
      mutable slider : GRange.scale;
      mutable area : GMisc.drawing_area }

type name_state = NORMAL | ZOOM | EDIT | PREVIOUS
type info_state =
    { name        : name_state ;
      activate    : infos:infos -> unit ;
      desactivate : infos:infos -> unit ;
      draw        : infos:infos -> cr:Cairo.t -> Cairo.t ;
      button_press     : infos:infos -> GdkEvent.Button.t -> bool ;
      button_release   : infos:infos -> GdkEvent.Button.t -> bool ;
      mouse_motion     : infos:infos -> GdkEvent.Motion.t -> bool ;
      mouse_scroll     : infos:infos -> GdkEvent.Scroll.t -> bool }
and states =
    { mutable current : info_state ;
      mutable prev    : info_state ;
      normal : info_state ;
      zoom   : info_state ;
      edit   : info_state }
and infos = 
    { states : states;
      window : window;
      mouse  : mouse;
      widgets: widgets }


(* References sur les params d'affichage de la mcell *)
let current_mcell = ref (Mcell.empty 2)
let current_i = ref 0
let current_j = ref 0
let current_t = ref 2
let current_scale = ref 4.
let is_animated = ref false
let animate_frame_delay = 40

module MSnap = Map.Make(struct type t = int let compare = compare end)

let snapshots = ref MSnap.empty 
let _ = snapshots := MSnap.add 0 !current_mcell !snapshots

let current_iter = ref 0
let max_iter = ref 0


(** Fonctions liées aux fichiers Rle/Mc/Mcop **)

(* Affiche la localisation d'une erreur de lexer/parser *)
let print_loc fmt lexbuf =
  let pos = Lexing.lexeme_start_p lexbuf in
  Format.fprintf fmt "File \"%s\", line %d:@\n" 
    pos.Lexing.pos_fname pos.Lexing.pos_lnum  


(* Charge une mcell depuis un fichier*)
let file_to_mcell filename =
  if (Filename.check_suffix filename ".rle") then Rle.read filename
  else Mc_file.read filename 


(* Crée un fichier à partir d'une mcell*)
let mcell_to_file filename m =
  if (Filename.check_suffix filename ".rle") then Rle.write m filename
  else Mc_file.write m filename 


(* Execute un ficher mcop, les actions sont déterminées
   par les trois fonctions fd frle fmc *)
let execute_mcop filename fd frle fmc =
  let file = open_in filename in
  let lexbuf = Lexing.from_channel file in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  let decl, statement = 
    try
      Mcop_parser.file Mcop_lexer.token lexbuf 
    with
        Parsing.Parse_error -> 
          Format.eprintf "%aSyntax error@." print_loc lexbuf;
          exit 1
      | Mcop_lexer.Lexical_error s -> 
        Format.eprintf "%aLexical error: %s@." print_loc lexbuf s;
        exit 1
  in
  
  let map = ref M.empty in

  let rec eval_expr = function
    | EVar i -> (try M.find i !map with Not_found -> failwith "p aie")
    | EUnion (e1,e2) -> Mcell.union (eval_expr e1) (eval_expr e2)
    | EInter (e1,e2) -> Mcell.inter (eval_expr e1) (eval_expr e2)
    | EDiff (e1,e2) -> Mcell.diff (eval_expr e1) (eval_expr e2)
    | EReadRle s -> Rle.read s
    | EReadMc s -> Mc_file.read s
    | EIter (e,t) -> Mcell.iter ~t (eval_expr e)
    | EMirror e -> Mcell.mirror (eval_expr e)
    | ERot90 e -> Mcell.rotate90 (eval_expr e)
    | ERot180 e -> Mcell.rotate180 (eval_expr e)
    | ERot270 e -> Mcell.rotate270 (eval_expr e)
    | EExtend e -> Mcell.extend (eval_expr e)
    | ELxtd e -> Mcell.extend_left (eval_expr e)
    | ERxtd e -> Mcell.extend_right (eval_expr e)
  in
  
  let rec eval_decl = function
    | [] -> ()
    | (i,e)::l' ->
      map := M.add i (eval_expr e) !map;
      eval_decl l'
  in 

  eval_decl decl;
  begin
    match statement with
      | Draw e -> fd (eval_expr e)
      | OutRle (e,s) -> frle (eval_expr e) s
      | OutMc (e,s) -> fmc (eval_expr e) s
  end;
  clear_hashtbl HUNION;
  clear_hashtbl HINTER;
  clear_hashtbl HDIFF;
  clear_hashtbl HMIROR;
  clear_hashtbl HROT90;
  clear_hashtbl HROT180;
  clear_hashtbl HROT270 














(** Fonctions liées aux widgets Gtk **)

(* Permet de changer l'état de l'interface graphique *)
let switch_state infos name_state =
  infos.states.current.desactivate ~infos;
  let prev = infos.states.prev in
  infos.states.prev <- infos.states.current;
  begin
    match name_state with
      | PREVIOUS -> 
          infos.states.current <- prev
      | NORMAL ->
          infos.states.current <- infos.states.normal
      | ZOOM ->
          infos.states.current <- infos.states.zoom
      | EDIT ->
          infos.states.current <- infos.states.edit
  end;
  infos.states.current.activate ~infos;
  ()

(* Permet de rafraichir l'affichage d'un widget *)
let refresh w =
  GtkBase.Widget.queue_draw w#as_widget

(* Permet de cacher un widget *)
let hide_widget w =
  GtkBase.Widget.hide w#as_widget

(* Permet de ré-afficher un widget *)
let show_widget w =
  GtkBase.Widget.show w#as_widget

(* Cache ou affiche un widget *)
let display_widget w b =
  if b then show_widget w
  else hide_widget w

(* Retourne vrai si le widget est affiché *)
let is_widget_displayed w =
  w#misc#visible

(* Retourne la taille d'un widget *)
let get_widget_size w =
  let { Gtk.width = width; Gtk.height = height } =
    w#misc#allocation
  in
  width, height


(* Permet de changer le label du bouton pause en fonction
   de l'état de l'animation *)
let switch_label_button_pause infos =
  let button = infos.widgets.button_pause in
  if not !is_animated then button#set_label "Pause"
  else button#set_label "Animer"


let set_label_t infos t =
  infos.widgets.label_t#set_text ("speed = "^(string_of_int t))

let set_label_i infos i = 
  infos.widgets.label_i#set_text ("i = "^(string_of_int i))

let set_label_j infos j = 
  infos.widgets.label_j#set_text ("j = "^(string_of_int j))

let set_label_scale infos scale = 
  infos.widgets.label_scale#set_text ("scale = "^(string_of_float scale))

let set_label_pop infos pop =
  infos.widgets.label_pop#set_text ("population = "^(string_of_int pop))



let update_slider infos =
  let i = float_of_int (!current_iter) in
  if (!current_iter = !max_iter) then
    infos.widgets.slider#adjustment#set_bounds 
      ~lower:0. ~upper:i
      ~step_incr:1. () ;
  infos.widgets.slider#adjustment#set_value i










(** Fonctions pour changer l'affichage de la macro-cell **)
(* Recompte le nombre de cell vivante et met à jour le label *)
let display_count_live_cells infos =
  if is_widget_displayed infos.widgets.label_pop then
    let count = Mcell.count_live_cells !current_mcell in
    set_label_pop infos count

let incr_iter t =
  current_iter := !current_iter + t
  
let incr_max_iter t =
  if !current_iter = !max_iter then
    max_iter := !max_iter + t

let add_snapshot () =
  let t = 1 lsl ((Mcell.size !current_mcell) -1) in
  let timer = (!current_iter mod t) - !current_t in
  if timer <= 0 then 
    begin
      (* Format.printf "Timer ajouté à %d, timer : %d, iter : %d @." *)
      (*   !current_iter t !current_iter; *)
      snapshots := MSnap.add !current_iter !current_mcell !snapshots;
    end


let raz_snapshots m =
  (* Format.printf "RAZ snapshots@."; *)
  current_iter := 0;
  max_iter := 0;
  snapshots := MSnap.empty;
  snapshots := MSnap.add 0 m !snapshots 



let go_back_to n infos = 
  let m =
    try
      MSnap.find n !snapshots
    with
        Not_found ->
          (* cherche le snapshot le plus proche de l'itération
             demandée *)
          let best = 
            MSnap.fold 
              (fun k _ i ->
                let diff = n - k in
                if diff < 0 then i 
                else if diff = 0 then k (* impossible *)
                else if diff > 0 && n - i > diff then k
                else i)
              !snapshots 0
          in
          (* Format.printf "Meilleure it : %d@." best; *)
          let start = MSnap.find best !snapshots in
          (* Format.printf "size : %d@." start.size; *)
          iter ~t:(n - best) start
  in
  (* adapte l'affichage si la mcell est de taille différente *)
  let i, j =
    let size_avant = 1 lsl Mcell.size !current_mcell in
    let size_apres = 1 lsl Mcell.size m in
    let dec = (size_apres - size_avant) / 2 in
    !current_i+dec, !current_j+dec
  in
  current_mcell := m;
  current_i := i;
  current_j := j;
  (* Format.printf "size : %d@." !current_mcell.size; *)
  current_iter := n;
  refresh infos.widgets.area;
  true



(* Lance une iteration d'un pas de temps t sur la mcell
   "current_mcell" et l'affiche *)
let iterate_mcell ?(t=(!current_t)) infos () =
  (* Pour actualiser le dessin en mode ZOOM *)
  infos.window.background <- None;
  (* Calculs pour l'itération *)
  let m = !current_mcell in
  let i, j, m =
    let size_avant = 1 lsl Mcell.size m in
    let m = Mcell.iter ~t m in
    let size_apres = 1 lsl Mcell.size m in
    let dec = (size_apres - size_avant) / 2 in
    !current_i+dec, !current_j+dec, m
  in
  current_mcell := m;
  current_i := i;
  current_j := j;
  incr_max_iter t;
  incr_iter t;
  (* Format.printf "%d %d@." !current_iter !max_iter; *)
  add_snapshot ();
  update_slider infos;
  (* Refresh de l'affichage *)
  display_count_live_cells infos;
  refresh infos.widgets.area;
  true


(* Fonction Timer pour lancer l'animation de la mcell *)
(** Inspiré de l'exemple cube.ml de Cairo **)
let timeout = ref None
let animate infos () =
  switch_label_button_pause infos;

  let animate_aux infos _ =
    if not infos.mouse.dragged then
      iterate_mcell infos ()
    else true
  in

  begin
    match !timeout with
      | None when not !is_animated ->
	begin
          is_animated := true;
  	  timeout := Some ( Glib.Timeout.add animate_frame_delay
  			      (animate_aux infos))
	end
      | Some id when !is_animated ->
  	begin
	  Glib.Timeout.remove id;
          is_animated := false;
  	  timeout := None
	end
      | _ -> ()
  end;
  true


(* Donne l'entier à ajouter ou soustraire à i ou j pour déplacer
   correctement la fenêtre selon le scale *)
let move () = 
  let scale = !current_scale in
  if scale < 1. then truncate (1. /. scale *. 4.)
  else 2

(* Change le i de la fenêtre *)
let change_i di infos () =
  let i = !current_i - di in
  current_i := i;
  set_label_i infos i;
  refresh infos.widgets.area;
  true

(* Change le j de la fenêtre *)
let change_j dj infos () =
  let j = !current_j + dj in
  current_j := j;
  set_label_j infos j;
  refresh infos.widgets.area;
  true

(* Change la vitesse d'animation *)
let change_t dir infos () =
  let t = !current_t in
  let new_t =
    if dir then max t (t*2)
    else max 1 (t/2)
  in
  if not ( new_t = t ) then
    begin
      current_t := new_t;
      set_label_t infos new_t
    end;
  false


(*
  - si i<>-1 && j<>-1 alors le zoom s'effectue avec comme repère le curseur
  - sinon le repère est le centre de la fenêtre
  - mode = true signifie que ce qui est pointé par le curseur sera placé au
     centre de la fenêtre après le zoom
  - mode = false signifie que ce qui est pointé par le curseur le sera encore
     après le zoom
 *)
let zoom ?(i= -1) ?(j= -1) ?(mode=true) ?(sensibility=2.) infos () =
  assert ( i >= -1 );
  assert ( j >= -1 );
  let scale = !current_scale in
  let new_scale = scale *. sensibility in
  (* Si on a atteint le max int alors on ne fait rien *)
  if not ( new_scale < scale ) then
    begin
      (* On change le scale pour zoomer *)
      current_scale := new_scale;
      set_label_scale infos new_scale;
      let width, height = get_widget_size infos.widgets.area in
      let (width, height) =
        max 1. ((float width) /. scale),
        max 1. ((float height) /. scale)
      in
      (* Si i=-1 et j = -1 alors on centre le zoom au milieu de la fenetre *)
      if ( i = -1 && j = -1 ) then
	begin
	  let new_w = width /. sensibility in
	  let new_h = height /. sensibility in
	  current_i := truncate
	    ((float !current_i) +. (width /. 2.) -. (new_w /. 2.));
	  current_j := truncate 
	    ((float !current_j) +. (height /. 2.) -. (new_h /. 2.))
	end
      (* Sinon on centre selon le i et le j et le mode *)
      else
	if mode then
	  begin
	    let new_w = width /. sensibility in
	    let new_h = height /. sensibility in
	    current_i := truncate
	      ((float !current_i) +. (float i) -. (new_w /. 2.));
	    current_j := truncate
	      ((float !current_j) +. (height-.(float j)) -. (new_h /. 2.))
	  end
	else 
	  begin
	    current_i := !current_i + i/2;
	    current_j := !current_j + ((truncate height)-j)/2
	  end;
      set_label_i infos !current_i;
      set_label_j infos !current_j;
      refresh infos.widgets.area;
      true
    end
  else false


(*
  - si i<>-1 && j<>-1 alors le dézoom s'effectue avec comme repère le curseur
  - sinon le repère est le centre de la fenêtre
  - mode = true signifie que ce la fenêtre avant le dézoom sera placée au
     centre de la fenêtre après le dézoom
  - mode = false signifie que ce qui est pointé par le curseur le sera encore
     après le dézoom
 *)
let unzoom ?(i= -1) ?(j= -1) ?(mode=true) infos () =
  assert ( mode || i >= -1);
  assert ( mode || j >= -1);
  let scale = !current_scale in
  current_scale := scale /. 2.;
  set_label_scale infos !current_scale;

  let width, height = get_widget_size infos.widgets.area in
  let (width, height) =
    max 1 (truncate ((float width) /. scale) ),
    max 1 (truncate ((float height) /. scale))
  in
  (* Si i=-1 et j=-1 ou selon le mode alors on dézoom depuis le milieu *)
  if ( mode || (i = -1 && j = -1) ) then
    begin
      current_i := !current_i - (width/2);
      current_j := !current_j - (height/2)
    end
  (* Sinon on dézoome depuis i et j *)
  else
    begin
      current_i := !current_i - i;
      current_j := !current_j - (height-j)
    end;
  set_label_i infos !current_i;
  set_label_j infos !current_j;
  refresh infos.widgets.area;
  true




















(** Dessin d'une macro-cell **)

let fill_rect ~cr ~x ~y ~w ~h =
  Cairo.save cr;
  begin
    Cairo.rectangle cr ~x ~y ~width:w ~height:h;
    Cairo.fill cr
  end;
  Cairo.restore cr;
  ()

let draw_point ~cr ~x ~y =
  fill_rect ~cr ~x ~y ~w:1. ~h:1.


(* Le traitement à faire s'il n'y a pas de découpage dans dessine *)
let rec execute_draw ~infos ~cr ~x ~y ~w ~h ~m ~i ~j ~scale =
  let { Gtk.width = _;  Gtk.height = area_height } =
    infos.widgets.area#misc#allocation
  in
  if (float w) *. scale <= 1. && not (Mcell.is_empty m) then
    let y = (float area_height) -. 1. -. y in
    draw_point ~cr ~x ~y
  else if Mcell.is_full m then
    begin
      let w = (float w) *. scale in
      let h = (float h) *. scale in
      let y = (float area_height) -. w -. y in
      fill_rect ~cr ~x ~y ~w ~h
    end
  else if not (Mcell.is_empty m) then
    draw_step ~infos ~cr ~x ~y ~w ~h ~m ~i ~j ~scale

and draw_step ~infos ~cr ~x ~y ~w ~h ~m ~i ~j ~scale =
  assert (i >= 0);
  assert (j >= 0);
  assert (x >= 0.);
  assert (y >= 0.);
  assert (w >= 0);
  assert (h >= 0);


  (* La moitié de la taille de la mcell *)
  let mid = (1 lsl ((Mcell.size m) - 1)) in

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
    execute_draw ~infos ~cr ~x ~y ~w ~h ~m ~i ~j ~scale
  (* Sinon on découpe et on appelle récursivement avec la meme cell m *)
  else    (* Si on doit découper que horizontalement *)
    if i_at_left = i2_at_left then
      begin
	let mcell_n = if i_at_left then m.nw else m.ne in
	let mcell_s = if i_at_left then m.sw else m.se in
	let i = if i_at_left then i else i-mid in
	let h1 = mid - j in
	let y2 = y +. (float h1) *. scale in
	let h2 = h - h1 in
	execute_draw ~infos ~cr ~x ~y    ~w ~h:h1 ~m:mcell_s ~i ~j   ~scale;
	execute_draw ~infos ~cr ~x ~y:y2 ~w ~h:h2 ~m:mcell_n ~i ~j:0 ~scale
      end
    (* Si on doit découper que verticalement *)
    else if j_at_bottom = j2_at_bottom then
      begin
	let mcell_e = if j_at_bottom then m.se else m.ne in
	let mcell_w = if j_at_bottom then m.sw else m.nw in
	let j = if j_at_bottom then j else j-mid in
	let w1 = mid - i in
	let x2 = x +. (float w1) *. scale in
      	let w2 = w - w1 in
	execute_draw ~infos ~cr ~x    ~y ~w:w1 ~h ~m:mcell_w ~i   ~j ~scale;
	execute_draw ~infos ~cr ~x:x2 ~y ~w:w2 ~h ~m:mcell_e ~i:0 ~j ~scale
      end
    (* Si on doit découper horizontalement et verticalement *)
    else
      begin
	let w1 = mid - i in
	let h1 = mid - j in
	let x2 = x +. (float w1) *. scale in
	let y2 = y +. (float h1) *. scale in
	let w2 = w - w1 in
	let h2 = h - h1 in
	execute_draw ~infos ~cr ~x    ~y    ~w:w1 ~h:h1 ~m:m.sw ~i   ~j   ~scale;
	execute_draw ~infos ~cr ~x:x2 ~y    ~w:w2 ~h:h1 ~m:m.se ~i:0 ~j   ~scale;
	execute_draw ~infos ~cr ~x    ~y:y2 ~w:w1 ~h:h2 ~m:m.nw ~i   ~j:0 ~scale;
	execute_draw ~infos ~cr ~x:x2 ~y:y2 ~w:w2 ~h:h2 ~m:m.ne ~i:0 ~j:0 ~scale
      end


let draw_mcell ~infos ~cr =
  Cairo.save cr;
  begin
      (* Initialisation de la matrice *)
    Cairo.identity_matrix cr;
    Cairo.set_source_rgb cr 0. 0. 0.;

      (* On traite la mcell *)
    let { Gtk.width = area_width; Gtk.height = area_height } =
      infos.widgets.area#misc#allocation
    in
    let m = !current_mcell in
    let scale = !current_scale in
    let i = !current_i in
    let j = !current_j in
    let w = max 1 (truncate ((float area_width) /. scale)) in
    let h = max 1 (truncate ((float area_height) /. scale)) in
      (* Si le i ou le j est complètement en dehors, on affiche rien *)
    if not ( i >= (1 lsl m.size) || j >= (1 lsl m.size) ) then
      begin
        let size = 1 lsl m.size in
        let x,w,i =
          if i < 0 then
            (float (-i)) *. scale, w + i, 0
          else 0., w, i
        in
        let y,h,j =
          if j < 0 then
            (float (-j)) *. scale, h + j, 0
          else 0., h, j
        in
        let w = min w (size - i) in
        let h = min h (size - j) in
        draw_step ~infos ~cr ~x ~y ~w ~h ~m ~i ~j ~scale
      end;
  end;
  Cairo.restore cr;
  cr















(** Fonctions communes aux states **)

(* Effectue un round particulier autour de 0 *)
let round_around_0 n =
  if n > -1. && n <= -0.5 then -1
  else if n < 0.5 && n > -0.5 then 0
  else if n < 1. && n >= 0.5 then 1
  else truncate n

(* Modifie la position de la souris dans les infos *)
let set_user_mouse_pos ~infos ~x ~y =
  infos.mouse.mx <- x;
  infos.mouse.my <- y

let global_button_press ~infos _ =
  infos.mouse.button_clicked <- true;
  true

let global_button_release ~infos event =
  infos.mouse.button_clicked <- false;
  infos.mouse.dragged <- false;
  true

let global_mouse_motion ~infos event =
  let x = GdkEvent.Motion.x event in
  let y = GdkEvent.Motion.y event in
  let mouse = infos.mouse in
  (* Traitement du drag'n'drop *)
  if ( mouse.button_clicked ) then
    let scale = !current_scale in
    let dx = round_around_0 ((x -. mouse.mx) /. scale) in
    let dy = round_around_0 ((y -. mouse.my) /. scale) in
    if dx <> 0 || dy <> 0 then
      begin
	set_user_mouse_pos ~infos ~x ~y;
	infos.mouse.dragged <- true
      end;
    if dx <> 0 then ignore (change_i dx infos ());
    if dy <> 0 then ignore (change_j dy infos ());
    true
  (* Traitement normal du mouvement de la souris *)
  else
    begin
      set_user_mouse_pos ~infos ~x ~y;
      false
    end

let global_mouse_scroll ~infos event =
  let scale = !current_scale in
  let i = truncate ((GdkEvent.Scroll.x event) /. scale) in
  let j = truncate ((GdkEvent.Scroll.y event) /. scale) in
  match GdkEvent.Scroll.direction event with
    | `UP -> zoom infos ~i ~j ~mode:false ()
    | `DOWN -> unzoom infos ~i ~j ~mode:false ()
    | _ -> false
