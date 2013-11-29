
open Common_fun

let activate ~infos =
  infos.widgets.button_edit#set_label "Return in Normal Mode";
  ()

let desactivate ~infos =
  infos.widgets.button_edit#set_label "Switch to Edit Mode";
  ()

let color = ref true
let edit_point infos =
  let color = !color in
  let mouse = infos.mouse in

  (* On récupère la coordonnée en i,j dans la mcell *)
  let _, h = get_widget_size infos.widgets.area in
  let i = truncate (mouse.mx /. !current_scale) + !current_i in
  let j = truncate
    (((float h) -. mouse.my) /. !current_scale) + !current_j + 1 in
  let mcell_size = 1 lsl (Mcell.size !current_mcell) in
  let j = mcell_size - j in
  
  (* Et on fait la mise à jour *)
  current_mcell := Mcell.set_extend !current_mcell j i color;

  (* S'il y a eu un extend, on recentre *)
  let size_apres = 1 lsl (Mcell.size !current_mcell) in
  let dec = (size_apres - mcell_size) / 2 in
  current_i := !current_i + dec;
  current_j := !current_j + dec

let button_press ~infos event =
  infos.mouse.button_clicked <- true;
  let button = GdkEvent.Button.button event in

  (* Clic gauche = cell on, clic droit = cell off *)
  if button = 1 then color := true
  else color :=false;

  edit_point infos;

  refresh infos.widgets.area;
  raz_snapshots !current_mcell;
  update_slider infos;
  true


let mouse_motion ~infos event =
  let x = GdkEvent.Motion.x event in
  let y = GdkEvent.Motion.y event in
  infos.mouse.mx <- x;
  infos.mouse.my <- y;
  
  (* Si on clique en même temps alors on colorie pendant le mouvement *)
  if infos.mouse.button_clicked then
    begin
      edit_point infos;
      refresh infos.widgets.area
    end;

  false


(* let shortcuts_action ~infos = function *)
(*   | "e"        -> switch_state infos NORMAL *)
(*   | "f"        -> switch_state infos FILL *)
(*   | "Ctrl-z"   -> switch_state infos ZOOM *)
(*   | "p"        -> ignore (animate infos ()) *)
(*   | "z"        -> ignore (zoom infos ()) *)
(*   | "Shift-z"  -> ignore (unzoom infos ()) *)
(*   | _ -> () *)


let state_edit = {
  name = EDIT;
  activate = activate;
  desactivate = desactivate;
  draw = draw_mcell;
  button_press = button_press;
  button_release = global_button_release;
  mouse_motion = mouse_motion;
  mouse_scroll = global_mouse_scroll;
}

