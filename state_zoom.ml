
open Common_fun

(**
   - Gestion clic droit et clic gauche moche
 **)

let pause = ref false

let activate ~infos =
  infos.widgets.button_zoom#set_draw_indicator true;
  (* Si l'animation est en cours pendant qu'on va faire la sélection *)
  (*        du zoom, alors on met en pause l'animation *)
  if !is_animated && not !pause then
    begin
      ignore (animate infos ());
      pause := true
    end;
  refresh infos.widgets.area;
  ()

let desactivate ~infos =
  infos.widgets.button_zoom#set_draw_indicator false;
  (* Et si on avait mis en pause l'animation alors on la reprend *)
  if not !is_animated && !pause then
    begin
      ignore (animate infos ());
      pause := false
    end;
  infos.window.background <- None;
  refresh infos.widgets.area;
  ()

let draw ~infos ~cr =
  let { Gtk.width = width ; Gtk.height = height } =
    infos.widgets.area#misc#allocation
  in
  Cairo.save cr;

  let background =
    match infos.window.background with
      | None ->
	begin
	  let copy_surface = Cairo.surface_create_similar (Cairo.get_target cr)
	    Cairo.CONTENT_COLOR_ALPHA width height
	  in
	  let copy_cairo = Cairo.create copy_surface in
	  (* On va récupérer la fonction de draw du mcell pour récup le dessin *)
	  let draw_normal = infos.states.normal.draw in
  	  let surface = Cairo.get_target (draw_normal ~infos ~cr:copy_cairo) in
  	  infos.window.background <- Some (surface);
  	  surface
	end
      | Some surface -> surface
  in

  let sensibility = infos.window.zoom_sensibility in
  let mouse = infos.mouse in
  let width, height = float width, float height in
  let w = width /. sensibility in
  let h = height /. sensibility in
  let x = mouse.mx -. (w /. 2.) in
  let y = mouse.my -. (h /. 2.) in


  begin
    Cairo.identity_matrix cr;
      (* On réaffiche la mcell de fond qu'on avait sauvegardé *)
    Cairo.save cr;
    begin
      let pattern = Cairo.Pattern.create_for_surface background in
      Cairo.Pattern.set_extend pattern Cairo.EXTEND_REPEAT;
      Cairo.set_source cr pattern;
      Cairo.rectangle cr 0. 0. width height;
      Cairo.fill cr;
    end;
    Cairo.restore cr;

    Cairo.save cr; begin
      (* Et on dessine notre zone de sélection de zoom *)
      Cairo.set_source_rgb cr 0. 0. 0.;
      Cairo.rectangle cr x y w h;
      Cairo.stroke cr
    end;
    Cairo.restore cr;
  end;
  Cairo.restore cr;
  cr



let button_release ~infos event =
  let mouse = infos.mouse in
  infos.mouse.button_clicked <- false;
  (* S'il n'y a pas eu de drag'n'drop alors on zoom *)
  if not mouse.dragged then
    begin
      let button = GdkEvent.Button.button event in
      (* Clic gauche -> on zoom *)
      if ( button = 1 ) then
	begin
	  let i = truncate (mouse.mx /. !current_scale) in
	  let j = truncate (mouse.my /. !current_scale) in
	  let sensibility = infos.window.zoom_sensibility in
	  (* On retire le background pour forcer à redessiner *)
	  infos.window.background <- None;
	  zoom ~i ~j ~mode:true ~sensibility infos ()
	end
      (* Clic droite -> on dézoom *)
      else if ( button = 3 ) then
	begin
	  infos.window.background <- None;
	  unzoom ~mode:true infos ()
	end
      else false
    end
  else
    begin
      infos.mouse.dragged <- false;
      false
    end


let mouse_motion ~infos event =
  let b = global_mouse_motion ~infos event in
  if b then
    (* Si on a vrai, c'est qu'on est en drag'n'drop
       dans ce cas, on va forcer à redessiner la mcell *)
    infos.window.background <- None
  else
    (* Sinon on refresh la zone de sélection *)
    refresh infos.widgets.area;
  b


let mouse_scroll ~infos event =
  let cs = infos.window.zoom_sensibility in
  begin
    match GdkEvent.Scroll.direction event with
      | `UP -> infos.window.zoom_sensibility <- min 10. (cs +. 0.5)
      | `DOWN -> infos.window.zoom_sensibility <- max 1.5 (cs -. 0.5)
      | _ -> ()
  end;
  refresh infos.widgets.area;
  true


(* let shortcuts_action ~infos = function *)
(*   | "e"        -> switch_state infos EDIT *)
(*   | "f"        -> *)
(*     (switch_state infos PREVIOUS; *)
(*      switch_state infos FILL) *)
(*   | "Ctrl-z"   -> switch_state infos PREVIOUS *)
(*   | "z"        -> *)
(*     (infos.window.background <- None; *)
(*      ignore (zoom infos ())) *)
(*   | "Shift-z"  -> *)
(*     (infos.window.background <- None; *)
(*      ignore (unzoom infos ())) *)
(*   | _ -> () *)


let state_zoom = {
  name = ZOOM;
  activate = activate;
  desactivate = desactivate;
  draw = draw;
  button_press = global_button_press;
  button_release = button_release;
  mouse_motion = mouse_motion;
  mouse_scroll = mouse_scroll;
}
