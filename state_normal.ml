
open Common_fun


let activate ~infos =
  refresh infos.widgets.area;
  ()

(* let shortcuts_action ~infos = function *)
(*   | "e"        -> switch_state infos EDIT *)
(*   | "f"        -> switch_state infos FILL *)
(*   | "Ctrl-z"   -> switch_state infos ZOOM *)
(*   | "p"        -> ignore (animate infos ()) *)
(*   | "z"        -> ignore (zoom infos ()) *)
(*   | "Shift-z"  -> ignore (unzoom infos ()) *)
(*   | _ -> () *)


let state_normal = {
  name = NORMAL;
  activate = activate;
  desactivate = (fun ~infos:_ -> ());
  draw = draw_mcell;
  button_press = global_button_press;
  button_release = global_button_release;
  mouse_motion = global_mouse_motion;
  mouse_scroll = global_mouse_scroll;
}
