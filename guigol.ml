open Common_fun


(* Paramètres des zones de l'interface *)
let window_width = 1024
let window_heigth = 720

let user_widgets = {
  label_i=GMisc.label ();
  label_j=GMisc.label ();
  label_t=GMisc.label ();
  label_scale=GMisc.label ();
  label_pop=GMisc.label ();
  button_pause=GButton.button ();
  button_zoom=GButton.toggle_button ();
  button_edit=GButton.button ();
  slider=GRange.scale `HORIZONTAL ();
  area=GMisc.drawing_area ()
}
let user_window = {
  background=None ;
  zoom_sensibility=2.
}
let user_mouse = { 
  mx=0.;
  my=0.;
  button_clicked=false;
  dragged=false
} 

let state_normal = State_normal.state_normal
let user_states = {
  current = state_normal;
  prev = state_normal;
  normal = state_normal;
  zoom = State_zoom.state_zoom;
  edit = State_edit.state_edit
}

let infos = {
  states = user_states;
  window = user_window;
  mouse = user_mouse;
  widgets = user_widgets
}



let open_file_chooser_dialog actdialog stock action =
  let dialog = GWindow.file_chooser_dialog
    ~action:actdialog
    ~position:`CENTER_ON_PARENT
    ~destroy_with_parent:true
    ()
  in
  dialog#add_button_stock `CANCEL `CANCEL;
  dialog#add_select_button_stock stock `ACCEPT;
  if dialog#run () = `ACCEPT then
    begin
      match dialog#filename with 
        | Some s -> action s
        | None -> ()
    end;
  dialog#destroy ()

(* Initialise le scale de telle sorte que toute la mcell soit visible *)
let rec init_scale m =
  let size = (float_of_int (1 lsl (Mcell.size m))) in
  let width, height = get_widget_size infos.widgets.area in
  let scale = !current_scale in
  let maximum_size = min 
    ((float width) /. scale) 
    ((float height) /. scale)
  in
  if maximum_size < size then 
    begin
      current_scale := scale /. 2.;
      init_scale m
    end

(* Centre la mcell à l'affichage *)
let center_mcell m =
  current_i := 0;
  current_j := 0;
  current_scale := 4.;
  init_scale m;
  set_label_i infos 0;
  set_label_j infos 0;
  set_label_scale infos !current_scale


(* Bascule du mode ZOOM ou mode PREVIOUS selon notre state *)
let switch_zoom_mode () =
  match infos.states.current.name with
    | ZOOM -> ignore (switch_state infos PREVIOUS)
    | _ -> ignore (switch_state infos ZOOM)

(* Bascule vers le mode EDIT ou PREVIOUS selon notre state *)
let switch_edit_mode () =
  match infos.states.current.name with
    | ZOOM ->
        ignore (switch_state infos PREVIOUS);
        ignore (switch_state infos EDIT)
    | EDIT -> ignore (switch_state infos PREVIOUS)
    | _ -> ignore (switch_state infos EDIT)


(* Affecte la mcell m comme étant la mcell courante et refresh la fenetre *)
let set_mcell m =
  current_mcell := m;
  current_iter := 0;
  raz_snapshots !current_mcell;
  center_mcell m;
  let count = Mcell.count_live_cells !current_mcell in
  set_label_pop infos count;
  refresh infos.widgets.area

(* Initialise une nouvelle mcell et l'affiche *)
let new_mcell () =
  let m = Mcell.empty 4 in
  set_mcell m

(* Demande à l'utilisateur de choisir un fichier et affiche la mcell *)
let open_file () =
  let action = fun s ->
    let m = Common_fun.file_to_mcell s in
    set_mcell m
  in
  open_file_chooser_dialog `OPEN `OPEN action

(* Sauvegarde la mcell courante dans un fichier *)
let save_mcell () =
  let action = fun s ->
    Common_fun.mcell_to_file s !current_mcell
  in
  open_file_chooser_dialog `SAVE `SAVE action


(* Ouvre un fichier d'exemple et l'affiche la mcell *)
let open_example s =
  let m = Common_fun.file_to_mcell ("examples/"^s) in
  set_mcell m

(* Demande dans combien de pas de temps l'utilisation veut aller *)
let rec jump_in_time () =
  let s = GToolbox.input_string
    ~title:"Jump in time"
    ~ok:"Jump"
    ~cancel:"Don't jump"
    ~text:"1"
    "How many step do you want to jump ? (Must be a number over 0)"
  in
  match s with
    | Some s ->
      begin
        try 
          let t = int_of_string s in
          ignore(iterate_mcell ~t infos ())
        with _ -> jump_in_time ()
      end
    | None -> ()


let go_back_slider () =
  let step = int_of_float user_widgets.slider#adjustment#value in
  ignore(go_back_to step infos)

(* Demande de combien d'itérations retourner *)
let rec go_back_in_time () =
  let s = GToolbox.input_string
    ~title:"Go back in time"
    ~ok:"Jump"
    ~cancel:"Don't jump"
    ~text:"1"
    "How many steps do you want to go back from ? (Must be a number over 0)"
  in
  match s with
    | Some s ->
      begin
        try 
          let t = int_of_string s in
          let t = !current_iter - t in
          let t = if t > 0 then t else 0 in
          ignore(go_back_to t infos)
        with _ -> go_back_in_time ()
      end
    | None -> ()

(* Demande à l'utilisation la taille de la carpet et l'affiche *)
let rec make_carpet () =
  let s = GToolbox.input_string
    ~title:"Pattern : Sierpinski carpet"
    ~ok:"Generate"
    ~text:"4"
    "Size of the fractal :"
  in
  match s with
    | Some s ->
      begin
        try
          let n = int_of_string s in
          let m = Mcell.carpet n in
          set_mcell m
        with _ -> make_carpet ()
      end
    | None -> ()


(* Sauvegarde l'écran dans un fichier image *) 
let export_to_png () =
  let action = fun filename ->
    let area = infos.widgets.area in
    let w,h = get_widget_size area in
    let cr = Cairo_lablgtk.create area#misc#window in
    let copy_surface = Cairo.surface_create_similar
      (Cairo.get_target cr) Cairo.CONTENT_COLOR_ALPHA w h 
    in
    let cr = Cairo.create copy_surface in
    Cairo.set_source_rgb cr 1. 1. 1.;
    Cairo.rectangle cr ~x:0. ~y:0.
    ~width:(float w) ~height:(float h);
    Cairo.fill cr;
    ignore (draw_mcell ~infos ~cr);

    let out = open_out filename in
    Cairo_png.surface_write_to_channel
      (Cairo.get_target cr) out;
    close_out out
  in
  open_file_chooser_dialog `SAVE `SAVE action


(* Exporte ma mcell dans un fichier .dot *)
let export_to_dot () =
  let action = fun s ->
    Mcell.dot s !current_mcell
  in
  open_file_chooser_dialog `SAVE `SAVE action


let global_keybindings = [
  GdkKeysyms._Up,       (fun i -> change_j (move ()) i);
  GdkKeysyms._Down,     (fun i -> change_j (-move ()) i);
  GdkKeysyms._Left,     (fun i -> change_i (move ()) i);
  GdkKeysyms._Right,    (fun i -> change_i (-move ()) i);
  GdkKeysyms._KP_Add,   (fun i -> change_t true i);
  GdkKeysyms._KP_Subtract,(fun i -> change_t false i);
  GdkKeysyms._e, (fun i () -> switch_edit_mode (); true)
]


let draw_window ~window _ =
  let cr = Cairo_lablgtk.create window#misc#window in
  ignore (user_states.current.draw ~infos ~cr);
  true

let event_button_zoom button () =
  (* Attention, quand on clique dessus pour passer en mode zoom
     alors le bouton est donc actif *)
  if not button#active then
    switch_state infos PREVIOUS
  else switch_state infos ZOOM

let event_button_edit () =
  match infos.states.current.name with
    | EDIT -> switch_state infos NORMAL
    | _ -> switch_state infos EDIT
    


let event_mouse_motion event =
  infos.states.current.mouse_motion ~infos event
let event_button_press event =
  infos.states.current.button_press ~infos event
let event_button_release event =
  infos.states.current.button_release ~infos event
let event_mouse_scroll event =
  infos.states.current.mouse_scroll ~infos event
let event_keyboard_press event =
  try
    let action =
      List.assoc
        (GdkEvent.Key.keyval event)
        global_keybindings
    in
    action infos ()
  with
       Not_found -> false


let create_menu label menubar =
  let item = GMenu.menu_item ~label ~packing:menubar#append () in
  GMenu.menu ~packing:item#set_submenu ()


module K = GdkKeysyms
let main mcell =
  (* On place la mcell dans notre reference *)
  current_mcell := mcell;

  (* Création des composants de l'interface *)
  let window = GWindow.window
    ~title:"Hashlife - by Pierrick COUDERC et David MAISON"
    ~width:window_width
    ~height:window_heigth
    ~allow_shrink:true
    ()
  in
  let layout = GPack.vbox ~packing:window#add () in

  (* La barre de Menu *)
  let menubar = GMenu.menu_bar ~height:25 ~packing:layout#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in

  let menu_file = factory#add_submenu "File" in
  let menu_navigate = factory#add_submenu "Navigate" in
  let menu_display = factory#add_submenu "Display" in
  let menu_pattern = factory#add_submenu "Pattern" in

  (* Le menu File *)
  let factory = new GMenu.factory
    menu_file
    ~accel_group
  in
  let factory2 = new GMenu.factory
    menu_file
    ~accel_group
    ~accel_modi:[]
  in
  ignore (factory#add_item "New mcell" ~key:K._n ~callback:
            (fun () -> set_mcell (Mcell.empty 4)));
  ignore (factory#add_item "Open file" ~key:K._o ~callback:open_file);
  ignore (factory#add_item "Save as..." ~key:K._s ~callback:save_mcell);
  ignore (factory#add_separator ());
  ignore (factory#add_item "Export to PNG" ~key:K._x ~callback:export_to_png);
  ignore (factory#add_item "Export to DOT" ~key:K._d ~callback:export_to_dot);
  ignore (factory#add_separator ());
  ignore (factory2#add_item "Quit" ~key:K._Escape ~callback:GMain.quit);

  (* Le menu Navigate *)
  let factory = new GMenu.factory 
    menu_navigate
    ~accel_group
    ~accel_modi:[]
  in
  let factory2 = new GMenu.factory 
    menu_navigate 
    ~accel_group 
    ~accel_modi:[`SHIFT]
  in
  let factory3 = new GMenu.factory 
    menu_navigate 
    ~accel_group 
  in
  ignore (factory3#add_item "Next step" ~key:K._space
            ~callback:(fun () -> ignore(iterate_mcell ~t:1 infos ())));
  ignore (factory#add_item "Iterate" ~key:K._space
            ~callback:(fun () -> ignore(iterate_mcell infos ())));
  ignore (factory#add_item "Animate/Pause" ~key:K._p
            ~callback:(fun () -> ignore(animate infos ())));
  ignore (factory#add_item "Jump in time..." ~key:K._j
            ~callback:jump_in_time);
  ignore (factory#add_item "Go back in time" ~key:K._r
            ~callback:go_back_in_time);
  ignore (factory#add_separator ());
  ignore (factory#add_item "Speed up" ~key:K._plus
            ~callback:(fun () -> ignore(change_t true infos ())));
  ignore (factory#add_item "Speed down" ~key:K._minus
            ~callback:(fun () -> ignore(change_t false infos ())));
  ignore (factory#add_separator ());
  ignore (factory#add_item "Zoom in" ~key:K._z
            ~callback:(fun () -> ignore(zoom infos ())));
  ignore (factory2#add_item "Zoom out" ~key:K._z
            ~callback:(fun () -> ignore(unzoom infos ())));
  ignore (factory3#add_item "Zoom mode" ~key:K._z
            ~callback:switch_zoom_mode);
  ignore (factory#add_separator ());
  ignore (factory#add_item "Center mcell" ~key:K._c
            ~callback:(fun () -> center_mcell !current_mcell));
  

  (* Menu Display *)
  let factory = new GMenu.factory menu_display in
  ignore (factory#add_check_item "Coord I"
            ~active:true
            ~callback:(fun b -> display_widget infos.widgets.label_i b));
  ignore (factory#add_check_item "Coord J"
            ~active:true
            ~callback:(fun b -> display_widget infos.widgets.label_j b));
  ignore (factory#add_check_item "Scale"
            ~active:true
            ~callback:(fun b -> display_widget infos.widgets.label_scale b));
  ignore (factory#add_check_item "Speed"
            ~active:true
            ~callback:(fun b -> display_widget infos.widgets.label_t b));
  ignore (factory#add_check_item "Population"
            ~active:true
            ~callback:(fun b -> display_widget infos.widgets.label_pop b));


  (* Menu Pattern *)
  let factory = new GMenu.factory
    menu_pattern
    ~accel_group
    ~accel_modi:[`CONTROL; `MOD1]
  in
  ignore (factory#add_item "Glider" ~key:K._g
            ~callback:(fun () -> open_example "glider.rle"));
  ignore (factory#add_item "Puffer" ~key:K._p
            ~callback:(fun () -> open_example "puffer.rle"));
  ignore (factory#add_item "Ticker" ~key:K._t
            ~callback:(fun () -> open_example "ticker.rle"));
  ignore (factory#add_item "Hexadecimal" ~key:K._h
            ~callback:(fun () -> open_example "hexadecimal.mc"));
  ignore (factory#add_item "Sierpinski carpet" ~key:K._c
            ~callback:make_carpet);

  (* On link l'accel_group à la fenêtre *)
  window#add_accel_group accel_group;


  (* GToolbox.create_shortcuts ~window ~shortcuts:global_shortcuts ~callback:shortcuts_action; *)

  (* Le conteneur pour les textes d'infications *)
  let hbox_text = GPack.hbox
    ~height:25
    ~packing:layout#pack
    ()
  in

  (* La drawing area pour la mcell *)
  let area = GMisc.drawing_area
    ~packing:(layout#pack ~expand:true) ()
  in

  let slider = GRange.scale `HORIZONTAL 
      ~draw_value:false ~packing:layout#pack
      ~update_policy:`DISCONTINUOUS () in
  
  slider#set_draw_value true;
  slider#adjustment#set_bounds 
    ~lower:0. ~upper:100.
    ~step_incr:1. () ;


  (* Le conteneur de nos boutons *)
  let pack_button = GPack.button_box
    `HORIZONTAL
    ~height:50
    ~child_width:100
    ~packing:layout#pack
    ()
  in

  (* Les zones de texte d'indication *)
  let label_i = GMisc.label
    ~text:("i = "^(string_of_int !current_i))
    ~packing:hbox_text#add
    ()
  in
  let label_j = GMisc.label
    ~text:("j = "^(string_of_int !current_j))
    ~packing:hbox_text#add
    ()
  in
  let label_scale = GMisc.label
    ~text:("scale = "^(string_of_float !current_scale))
    ~packing:hbox_text#add
    ()
  in
  let label_t = GMisc.label
    ~text:("speed = "^(string_of_int !current_t))
    ~packing:hbox_text#add
    ()
  in
  let label_pop = GMisc.label
    ~text:("population = 8")
    ~packing:hbox_text#add
    ()
  in

  (* Les boutons de la barre du bas *)
  let button_edit = GButton.button
    ~label:"Switch to Edit Mode"
    ~packing:pack_button#pack ()
  in
  let button_pause = GButton.button
    ~label:"Animer"
    ~packing:pack_button#pack ()
  in
  let pack_button_zoom = GPack.button_box
    `HORIZONTAL
    ~child_width:25
    ~packing:pack_button#pack () in
  let button_zoom_minus = GButton.button
    ~label:"-"
    ~packing:pack_button_zoom#pack ()
  in
  let button_zoom = GButton.toggle_button
    ~label:"Zoom"
    ~packing:pack_button_zoom#pack ()
  in
  let button_zoom_plus = GButton.button
    ~label:"+"
    ~packing:pack_button_zoom#pack ()
  in


  (* Les actions des boutons *)
  ignore (button_pause#connect#clicked (fun () ->
    ignore(animate infos ()) ));
  ignore (button_zoom#connect#clicked (event_button_zoom button_zoom));
  ignore (button_zoom_minus#connect#clicked (fun _ ->
    user_window.background <- None;
    ignore(unzoom infos ())
  ));
  ignore (button_zoom_plus#connect#clicked (fun _ ->
    user_window.background <- None;
    ignore(zoom infos ())
  ));
  ignore (button_edit#connect#clicked (event_button_edit));
  
  ignore (slider#connect#value_changed (go_back_slider)) ;

  (* Mise à jour de la variable globale user_widgets *)
  user_widgets.label_i <- label_i;
  user_widgets.label_j <- label_j;
  user_widgets.label_t <- label_t;
  user_widgets.label_scale <- label_scale;
  user_widgets.label_pop <- label_pop;
  user_widgets.button_pause <- button_pause;
  user_widgets.button_zoom <- button_zoom;
  user_widgets.button_edit <- button_edit;
  user_widgets.slider <- slider;
  user_widgets.area <- area;

  (* Les fonctions callback liés aux composants *)
  ignore (window#connect#destroy GMain.quit);
  ignore (area#misc#set_can_focus true);
  ignore (window#event#add [`KEY_PRESS; `KEY_RELEASE]);
  ignore (area#event#add
            [`BUTTON_PRESS; `BUTTON_RELEASE; `POINTER_MOTION; `SCROLL]);
  ignore (area#event#connect#button_press (event_button_press));
  ignore (area#event#connect#button_release (event_button_release));
  ignore (area#event#connect#motion_notify (event_mouse_motion));
  ignore (area#event#connect#scroll (event_mouse_scroll));
  ignore (area#event#connect#expose (draw_window ~window:area));
  ignore (window#event#connect#key_press (event_keyboard_press));
  (* ignore (window#event#connect#key_release
  (event_keyboard_release)); *)

  (* On lance l'affichage de la fenetre *)
  area#misc#realize ();
  init_scale !current_mcell;
  set_label_scale infos !current_scale;
  let count = Mcell.count_live_cells !current_mcell in
  set_label_pop infos count;

  window#show ();
  GMain.main ()
