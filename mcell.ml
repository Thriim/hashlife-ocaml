

type t =
  { id : int; size : int; nw : t; ne : t; sw : t; se : t; mutable result : t }
type mcell = t

type h =
    HCREATE | HUNION | HINTER | HDIFF
  | HMIROR | HROT90  | HROT180 | HROT270 
  | HCNTLIVE

module T = struct
  type t = mcell
  let equal m1 m2 =
    m1.nw == m2.nw && m1.ne == m2.ne && m1.sw == m2.sw && m1.se == m2.se
      
  let hash m = abs (19 * (19 * (19 * m.nw.id + m.ne.id) + m.sw.id) + m.se.id)
end
   
module T2 = struct
  type t = mcell * mcell
  let equal (m1, m2) (m1', m2') =
    m1 == m1' && m2 == m2'
  let hash (m1, m2) = abs (19 * m1.id + m2.id)
end

module H = Hashtbl.Make(T)
module H2 = Hashtbl.Make(T2)


let htbl_create = H.create 5003
let htbl_union = H2.create 5003
let htbl_inter = H2.create 5003
let htbl_diff = H2.create 5003
let htbl_miror = H.create 5003
let htbl_rot90 = H.create 5003
let htbl_rot180 = H.create 5003
let htbl_rot270 = H.create 5003 
let htbl_cntlive = Hashtbl.create 5003


let clear_hashtbl = function
  | HCREATE -> H.clear htbl_create
  | HUNION -> H2.clear htbl_union
  | HINTER -> H2.clear htbl_inter
  | HDIFF -> H2.clear htbl_diff
  | HMIROR -> H.clear htbl_miror
  | HROT90 -> H.clear htbl_rot90
  | HROT180 -> H.clear htbl_rot180
  | HROT270 -> H.clear htbl_rot270
  | HCNTLIVE -> Hashtbl.clear htbl_cntlive

let size_hashtbl = function
  | HCREATE -> H.length htbl_create
  | HUNION -> H2.length htbl_union
  | HINTER -> H2.length htbl_inter
  | HDIFF -> H2.length htbl_diff
  | HMIROR -> H.length htbl_miror
  | HROT90 -> H.length htbl_rot90
  | HROT180 -> H.length htbl_rot180
  | HROT270 -> H.length htbl_rot270
  | HCNTLIVE -> Hashtbl.length htbl_cntlive

let rec null = { id = -1; size = -1;
		 nw = null; ne = null; sw = null; se = null; result = null }
let on = { id = 1; size = 0;
	   nw = null; ne = null; sw = null; se = null; result = null }
let off = { id = 0; size = 0;
	    nw = null; ne = null; sw = null; se = null; result = null }


let rec log2 n = if n = 0 then -1 else 1 + log2 (n / 2)



let create = 
  let n = ref 2 in
  fun ~nw ~ne ~sw ~se ->
    let s = nw.size in
    assert (s = ne.size && s = sw.size && s = se.size);
    let new_t = { id = !n; size = s + 1;
		  nw = nw; ne = ne; sw = sw; se = se; result = null } in
    try
      H.find htbl_create new_t
    with Not_found ->
      H.add htbl_create new_t new_t;
      incr n;
      new_t
	
let size t = t.size

let max_size = 200

let empty_cells = Array.create max_size off
let full_cells = Array.create max_size on
let () =
  for i = 1 to max_size - 1 do
    (* On crée nos cellules vides/pleines *)
    let m = empty_cells.(i - 1) in
    let n = full_cells.(i - 1) in
    empty_cells.(i) <- create m m m m;
    full_cells.(i) <- create n n n n;
    (* Et on peut aussi déjà affecter leur result *)
    if i >= 2 then 
      begin
	empty_cells.(i).result <- empty_cells.(i-1);
      end
  done
  


let empty i = empty_cells.(i)
let is_empty m =
  assert (m != null);
  assert (m.size >= 0);
  assert (m.size < max_size);
  m == empty_cells.(m.size)

let full i = full_cells.(i)
let is_full m = m == full_cells.(m.size)

(* Extend une mcell de taille n en une de taille n+1 *)
let extend m =
  let e = empty (m.size - 1) in
  let nw = create ~nw:e ~ne:e ~sw:e ~se:m.nw in
  let ne = create ~nw:e ~ne:e ~sw:m.ne ~se:e in
  let sw = create ~nw:e ~ne:m.sw ~sw:e ~se:e in
  let se = create ~nw:m.se ~ne:e ~sw:e ~se:e in
  create nw ne sw se

(* Extend la mcell jusqu'à ce que les coord x,y soit dans la mcell *)
let rec extend_rec m x y =
  let s = 1 lsl m.size in
  let mcell =
    if x < 0 || x >= s || y < 0 || y >= s then
      extend m
    else
      m
  in
  let s' = 1 lsl (mcell.size - 2) in
  let x = if x < 0 then x + s' else x in
  let y = if y < 0 then y + s' else y in
  if mcell == m then m
  else extend_rec mcell x y
      


(* Place une cellule a l'endroit desire dans le Mcell *)
exception Unmodified
let set m x y bool =
  let rec aux m x y =
    let s = 1 lsl m.size in
    assert (x >= 0);
    assert (y >= 0);
    assert (x < s);
    assert (y < s);

    if (is_empty m && not bool) || (is_full m && bool) then
      if m.size >= 1 then raise Unmodified
      else m
    else if m == on then off
    else if m == off then on
    else
      let mid_size = 1 lsl (m.size - 1) in
      let north = if mid_size > x then true else false in
      let west = if mid_size > y then true else false in
      let x = if mid_size > x then x else x - mid_size in
      let y = if mid_size > y then y else y - mid_size in
      
      match north, west with
        | true, true -> 
          create ~nw:(aux m.nw x y) ~ne:m.ne ~sw:m.sw ~se:m.se
        | false, true -> 
          create ~nw:m.nw ~ne:m.ne ~sw:(aux m.sw x y) ~se:m.se
        | true, false -> 
          create ~nw:m.nw ~ne:(aux m.ne x y) ~sw:m.sw ~se:m.se
        | false, false -> 
          create ~nw:m.nw ~ne:m.ne ~sw:m.sw ~se:(aux m.se x y)
  in
  try
    aux m x y
  with
    Unmodified -> m

(* Idem que le set en extend si besoin la mcell si x ou y sorte de la mcell *)
let set_extend m x y bool =
  let prev_size = 1 lsl m.size in
  let m = extend_rec m x y in
  let actual_size = 1 lsl m.size in
  let dec = (actual_size - prev_size) / 2 in
  let x = x + dec in
  let y = y + dec in
  set m x y bool

let rec compute_result m =
  assert (m.size >= 2);
  (* Cas ou n=2, on applique les règles du jeu *)
  if m.size = 2 then
    (* On calcule le nombre de voisins on de chaque cell du result *)
    let v_nw = ref 0 in
    let v_ne = ref 0 in
    let v_sw = ref 0 in
    let v_se = ref 0 in
    if ( m.nw.nw == on ) then incr v_nw;
    if ( m.nw.ne == on ) then (incr v_nw; incr v_ne);
    if ( m.nw.sw == on ) then (incr v_nw; incr v_sw);
    if ( m.nw.se == on ) then (incr v_ne; incr v_sw; incr v_se);
    if ( m.ne.nw == on ) then (incr v_nw; incr v_ne);
    if ( m.ne.ne == on ) then incr v_ne;
    if ( m.ne.sw == on ) then (incr v_nw; incr v_sw; incr v_se);
    if ( m.ne.se == on ) then (incr v_ne; incr v_se);
    if ( m.sw.nw == on ) then (incr v_nw; incr v_sw);
    if ( m.sw.ne == on ) then (incr v_nw; incr v_ne; incr v_se);
    if ( m.sw.sw == on ) then incr v_sw;
    if ( m.sw.se == on ) then (incr v_sw; incr v_se);
    if ( m.se.nw == on ) then (incr v_nw; incr v_ne; incr v_sw);
    if ( m.se.ne == on ) then (incr v_ne; incr v_se);
    if ( m.se.sw == on ) then (incr v_sw; incr v_se);
    if ( m.se.se == on ) then incr v_se;
    (* On va maintenant créer les cell du result *)
    let nw =
      if ( m.nw.se == on && (!v_nw = 2 || !v_nw = 3) ) ||
	( m.nw.se == off && !v_nw = 3 ) then on
      else off
    in
    let ne =
      if ( m.ne.sw == on && (!v_ne = 2 || !v_ne = 3) ) ||
	( m.ne.sw == off && !v_ne = 3 ) then on
      else off
    in
    let sw =
      if ( m.sw.ne == on && (!v_sw = 2 || !v_sw = 3) ) ||
	( m.sw.ne == off && !v_sw = 3 ) then on
      else off
    in
    let se =
      if ( m.se.nw == on && (!v_se = 2 || !v_se = 3) ) ||
	( m.se.nw == off && !v_se = 3 ) then on
      else off
    in
    create nw ne sw se
  (* Sinon on va calculer recursivement avec les autres results *)
  else
    (* On va calculer les result des 4 coins *)
    let r_nw = result m.nw in
    let r_ne = result m.ne in
    let r_sw = result m.sw in
    let r_se = result m.se in
    (* Puis on va fabriquer les results en croix *)
    let r_n = result (create ~nw:m.nw.ne ~ne:m.ne.nw ~sw:m.nw.se ~se:m.ne.sw) in
    let r_w = result (create ~nw:m.nw.sw ~ne:m.nw.se ~sw:m.sw.nw ~se:m.sw.ne) in
    let r_e = result (create ~nw:m.ne.sw ~ne:m.ne.se ~sw:m.se.nw ~se:m.se.ne) in
    let r_s = result (create ~nw:m.sw.ne ~ne:m.se.nw ~sw:m.sw.se ~se:m.se.sw) in
    let r_c = result (create ~nw:m.nw.se ~ne:m.ne.sw ~sw:m.sw.ne ~se:m.se.nw) in
    (* Puis on assemble les morceaux pour créer ensuite le result final *)
    (* Ceci est un faux result qui fait une iter de 1 par 1 *)
    (* let nw = create ~nw:r_nw.se ~ne:r_n.sw ~sw:r_w.ne ~se:r_c.nw in *)
    (* let ne = create ~nw:r_n.se ~ne:r_ne.sw ~sw:r_c.ne ~se:r_e.nw in *)
    (* let sw = create ~nw:r_w.se ~ne:r_c.sw ~sw:r_sw.ne ~se:r_s.nw in *)
    (* let se = create ~nw:r_c.se ~ne:r_e.sw ~sw:r_s.ne ~se:r_se.nw in *)
    (* Ceci est le vrai result *)
    let nw = result (create ~nw:r_nw ~ne:r_n ~sw:r_w ~se:r_c) in
    let ne = result (create ~nw:r_n ~ne:r_ne ~sw:r_c ~se:r_e) in
    let sw = result (create ~nw:r_w ~ne:r_c ~sw:r_sw ~se:r_s) in
    let se = result (create ~nw:r_c ~ne:r_e ~sw:r_s ~se:r_se) in
    create nw ne sw se

and result t =
  if t.result != null then t.result
  else
    begin
      let r = compute_result t in
      t.result <- r;
      r
    end


(* Vérifie que la "couronne" extérieure est vide *)
let need_extend m =
  assert(m.size >= 2);
  not (is_empty m.nw.nw && is_empty m.nw.ne && is_empty m.ne.nw 
  && is_empty m.ne.ne && is_empty m.nw.sw && is_empty m.ne.se 
  && is_empty m.sw.nw && is_empty m.se.ne && is_empty m.sw.sw
  && is_empty m.sw.se && is_empty m.se.sw && is_empty m.se.se)


(* Diminue la taille de la mcell à n-1 si nécessaire *)
let simplify m =
  (* On n'acceptera pas de simplify une mcell de size < 3 car
     on ne pourrait appliquer le jeu de la vie dessus sans refaire un extend *)
  assert (m.size >= 3);
  (* on se sert de need_extend pour voir si la "couronne" est vide *)
  if not ( need_extend m ) then
    create ~nw:m.nw.se ~ne:m.ne.sw ~sw:m.sw.ne ~se:m.se.nw
  else m


(* Diminue la taille de la mcell autant que possible *)
let rec simplify_max m =
  (* Fonction de simplification max de base *)
  let go_simplify m = 
    let new_m = simplify m in
    if ( new_m == m ) then m
    else simplify_max new_m
  in

  (* On ne traite que les mcell de taille > 2 *)
  if ( m.size <= 2 ) then m
  (* Si c'est une grande mcell vide, on la simplify en une vide de taille 2 *)
  else if ( is_empty m ) then empty 2
  (* Si la taille est de 3 on ne cherche pas à optimiser dans les coins *)
  else if ( m.size = 3 ) then go_simplify m
  (* Sinon on optimise si possible les coins et on simplifie *)
  else go_simplify m



let center m =
  create ~nw:m.nw.se ~ne:m.ne.sw ~sw:m.sw.ne ~se:m.se.nw


(* Renvoie le futur d'une mcell *)
let rec future k m =
  assert (k <= (m.size-2) );
  assert (k >= 0);
  let n = m.size in

  (* Si la cellule est vide, alors son future aussi *)
  if ( is_empty m ) then
    empty (n-1)

  (* Cas de base *)
  else if ( k = n-2 ) then
    result m
  (* Sinon on fait une découpe et appels récursifs *)
  else
    (* On récupère déjà nos 9 sous parties *)
    let m_nw = center m.nw in
    let m_ne = center m.ne in
    let m_sw = center m.sw in
    let m_se = center m.se in
    let m_n = center (create ~nw:m.nw.ne ~ne:m.ne.nw ~sw:m.nw.se ~se:m.ne.sw) in
    let m_w = center (create ~nw:m.nw.sw ~ne:m.nw.se ~sw:m.sw.nw ~se:m.sw.ne) in
    let m_e = center (create ~nw:m.ne.sw ~ne:m.ne.se ~sw:m.se.nw ~se:m.se.ne) in
    let m_s = center (create ~nw:m.sw.ne ~ne:m.se.nw ~sw:m.sw.se ~se:m.se.sw) in
    let m_c = center (create ~nw:m.nw.se ~ne:m.ne.sw ~sw:m.sw.ne ~se:m.se.nw) in
    (* Puis on fait appel 4fois à future sur nos combinaisons des 9 *)
    let nw = future k (create ~nw:m_nw ~ne:m_n ~sw:m_w ~se:m_c) in
    let ne = future k (create ~nw:m_n ~ne:m_ne ~sw:m_c ~se:m_e) in
    let sw = future k (create ~nw:m_w ~ne:m_c ~sw:m_sw ~se:m_s) in
    let se = future k (create ~nw:m_c ~ne:m_e ~sw:m_s ~se:m_se) in
    create nw ne sw se

(* Lance une iteration du jeu de la vie sur une mcell m avec un pas de temps de t *)
let rec iter ?(t = 1) m =
  assert (t >= 1);
  let k = log2 t in
  let res =
    (* Si k<n-2 alors on extend 1 fois pour "avoir le future du centre"
       et +0/1 fois pour pas perdre des infos via un débordement *)
    if ( k <= (m.size - 2) ) then
      if need_extend m then
	future k (extend (extend m))
      else
	future k (extend m)
    (* Sinon on extend x fois pour ne rien perdre via des débordements *)
    else
      let rec aux m i =
	if ( i > 0 ) then aux (extend m) (i - 1)
	else m
      in
      future k (aux m (k-m.size+3))
  in

  let new_t = t - (1 lsl k) in
  if new_t > 0 then 
    iter ~t:new_t (simplify_max res)
  else
    simplify_max res


(* Prend 2 mcells t1 et t2 de taille n et m et retourne la mcell de taille
   max(n,m) qui utilise une opération entre t1 et t2
   Prend aussi une table de h qui sera utilisée pour le hashconsing *)
let rec mhash_op2 h t1 t2 f =
  let rec mhash_step t1 t2 =
    let t1, t2 =
      if t1.id < t2.id then t1, t2
      else t2, t1
    in
    assert (t1.size = t2.size );
    let new_t, b = f t1 t2 in
    if b then new_t
    else
      try
        H2.find h (t1, t2)
      with Not_found ->
        let new_t = create
          ~nw:(mhash_step t1.nw t2.nw)
          ~ne:(mhash_step t1.ne t2.ne)
          ~sw:(mhash_step t1.sw t2.sw)
          ~se:(mhash_step t1.se t2.se)
        in
        H2.add h (t1, t2) new_t;
        new_t
  in
  if t1.size > t2.size then mhash_op2 h t1 (extend t2) f
  else if t2.size > t1.size then mhash_op2 h (extend t1) t2 f
  else mhash_step t1 t2

let union t1 t2 =
  let f t1 t2 =
    if is_empty t1 then t2, true
    else if is_empty t2 then t1, true
    else if is_full t1 then t1, true
    else if is_full t2 then t2, true
    else null, false
  in
  mhash_op2 htbl_union t1 t2 f 

let inter t1 t2 =
  let f t1 t2 =
    if is_empty t1 then t1, true
    else if is_empty t2 then t2, true
    else if is_full t1 && is_full t2 then t1, true
    else null, false
  in
  mhash_op2 htbl_inter t1 t2 f

let diff t1 t2 =
  let f t1 t2 =
    if is_empty t1 && is_empty t2 then t1, true
    else if is_full t1 && is_full t2 then empty (t1.size), true
    else if is_full t1 && is_empty t2 then t1, true
    else if is_empty t1 && is_full t2 then t2, true
    else null, false
  in
  mhash_op2 htbl_diff t1 t2 f

let rec mirror =
  let h = H.create 17 in
  fun t ->
    if is_empty t || is_full t then t
    else
      try
        H.find h t
      with Not_found ->
        let new_t = create
          ~nw:(mirror t.ne)
          ~ne:(mirror t.nw)
          ~sw:(mirror t.se)
          ~se:(mirror t.sw)
        in
        H.add h t new_t;
        new_t

let rec rotate90 =
  fun t ->
    if is_empty t || is_full t then t
    else
      try
        H.find htbl_rot90 t
      with Not_found ->
        let new_t = create
          ~nw:(rotate90 t.ne)
          ~ne:(rotate90 t.se)
          ~sw:(rotate90 t.nw)
          ~se:(rotate90 t.sw)
        in
        H.add htbl_rot90 t new_t;
        new_t 

let rec rotate180 =
  fun t ->
    if is_empty t || is_full t then t
    else
      try
        H.find htbl_rot180 t
      with Not_found ->
        let new_t = create
          ~nw:(rotate180 t.se)
          ~ne:(rotate180 t.sw)
          ~sw:(rotate180 t.ne)
          ~se:(rotate180 t.nw)
        in
        H.add htbl_rot180 t new_t;
        new_t 

let rec rotate270 =
  fun t ->
    if is_empty t || is_full t then t
    else
      try
        H.find htbl_rot270 t
      with Not_found ->
        let new_t = create
          ~nw:(rotate270 t.sw)
          ~ne:(rotate270 t.nw)
          ~sw:(rotate270 t.se)
          ~se:(rotate270 t.ne)
        in
        H.add htbl_rot270 t new_t;
        new_t 

let extend_left m =
  let e1 = empty m.size in
  let e2 = empty (m.size -1) in
  let ne = create ~nw:e2 ~ne:e2 ~sw:m.nw ~se:m.ne in
  let se = create ~nw:m.sw ~ne:m.se ~sw:e2 ~se:e2 in
  create ~nw:e1 ~ne ~sw:e1 ~se

let extend_right m =
  let e1 = empty m.size in
  let e2 = empty (m.size -1) in
  let nw = create ~nw:e2 ~ne:e2 ~sw:m.nw ~se:m.ne in
  let sw = create ~nw:m.sw ~ne:m.se ~sw:e2 ~se:e2 in
  create ~nw ~ne:e1 ~sw ~se:e1

open Format

(* Ecrit le mcell au format .dot pour pouvoir visualiser le graphe *) 
let dot ~filename m =
  let h = Hashtbl.create 17 in
  let c = open_out filename in
  let fmt = formatter_of_out_channel c in
  fprintf fmt "digraph {@\nnode [shape = record];@\n";
  let rec visit m =
    if m != on && m != off && not (Hashtbl.mem h m.id) then begin
      Hashtbl.add h m.id ();
      fprintf 
        fmt 
        "n%d [label=\"{<id> S ize : %d |{<nw> NW|<ne> NE|<sw> SW|<se> SE}}\"];@\n" 
        m.id m.size;
      fprintf fmt "n%d:nw -> n%d;@\n" m.id m.nw.id;
      fprintf fmt "n%d:ne -> n%d;@\n" m.id m.ne.id;
      fprintf fmt "n%d:sw -> n%d;@\n" m.id m.sw.id;
      fprintf fmt "n%d:se -> n%d;@\n" m.id m.se.id;
      visit m.ne; visit m.nw; visit m.se; visit m.sw
    end
    else if m == on && not (Hashtbl.mem h m.id) then
      fprintf fmt "n%d [label=\"<id> ON\"];@\n" m.id
    else if m == off && not (Hashtbl.mem h m.id) then 
      fprintf fmt "n%d [label=\"<id> OFF\"];@\n" m.id
  in
  visit m;
  fprintf fmt "}@.";
  close_out c

(* Retourne un tapis de Sierpinski au format mcell *)
let rec carpet n =
  let t,e =
    if n > 1 then
      let t = carpet (n-2) in
      t, empty (t.size)
    else
      on, off
  in
  create
    ~nw:(create ~nw:t ~ne:t ~sw:t ~se:e)
    ~ne:(create ~nw:t ~ne:t ~sw:e ~se:t)
    ~sw:(create ~nw:t ~ne:e ~sw:t ~se:t)
    ~se:(create ~nw:e ~ne:t ~sw:t ~se:t)

(* Compte le nombre de cellules vivantes dans le mcell, et mémoïse le résultat *)
let rec count_live_cells m =
  if m == on then 1
  else if m == off then 0
  else
    try 
      Hashtbl.find htbl_cntlive m.id
    with 
        Not_found ->
          let c =
            (count_live_cells m.nw) + (count_live_cells m.ne) 
            + (count_live_cells m.sw) + (count_live_cells m.se)
          in
          Hashtbl.add htbl_cntlive m.id c;
          c

(* Compte le nombre de noeuds uniques dans la mcell *)
let count_nodes m =
  let h = Hashtbl.create 5003 in
  Hashtbl.add h on.id ();
  Hashtbl.add h off.id ();
  let rec count m = 
    if not (Hashtbl.mem h m.id) then
      let c =
        (count m.nw) + (count m.ne) 
        + (count m.sw) + (count m.se) + 1
      in
      Hashtbl.add h m.id ();
      c
    else 0
  in
  (count m) + 2
