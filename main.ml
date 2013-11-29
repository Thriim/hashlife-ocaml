
let dir = "files"^(Filename.dir_sep)
let ext = ".rle"
let ext_mc = ".mc"

open Mcop_ast

type op = Normal | Union | Inter | Diff
type out = Rle | Mc
let mode_op = ref false
let operation = ref Normal
let tests = ref false
let graphic = ref true
let time = ref 0
let filename2 = ref None
let out_type = ref Rle
let result_out = ref None
let dot = ref false
let filename_dot = ref None

let speclist = [
  ("-union", Arg.String(fun s -> filename2 := Some(s); operation := Union ),
   "Applique l'opération d'union sur deux fichiers mcell" );
  ("-inter", Arg.String(fun s -> filename2 := Some(s); operation := Inter ),
   "Applique l'opération d'intersection sur deux fichiers mcell" );
  ("-diff", Arg.String(fun s -> filename2 := Some(s); operation := Diff ),
   "Applique l'opération de différence sur deux fichiers mcell" );
  ("-test", Arg.Unit(fun () -> tests := true),
   "Lance les tests liés au module mcell");
  ("-c", Arg.Unit(fun () -> graphic := false),
   "Ne lance pas l'application graphique");
  ("-rle", Arg.String(fun s ->
    print_endline "rle"; result_out := Some(s)),
   "Enregistre le résultat dans un fichier .rle");
  ("-mc", Arg.String(fun s -> result_out := Some(s); out_type := Mc),
   "Enregistre le résultat dans un fichier .mc");
  ("-t", Arg.Int(fun i -> time := i),
   "Itère t fois le jeu de la vie sur la mcell résultante appelée
    (t>0 sinon ignoré)");
  ("-op", Arg.Unit(fun () -> mode_op := true), "Mode operation");
  ("-dot", Arg.String(fun s -> filename_dot := Some(s)),
   "Imprime le mcell dans un fichier .dot")
]


let main filename1 =
  (* On lance les tests si on l'a souhaité *)
  if !tests then Test.lancer_test ();

  (* Si on est pas en mode op, traitement normal *)
  if not !mode_op then
    begin
      (* On récupère la mcell à traiter en fonction de l'opération demandée *)
      let mcell =
        match !operation with
          | Normal -> Common_fun.file_to_mcell filename1
          | Union ->
              begin
                match !filename2 with
                  | None -> raise (Arg.Bad "Usage : -union [filename1] [filename2]")
                  | Some s ->
                      let m1 = Common_fun.file_to_mcell filename1 in
                      let m2 = Common_fun.file_to_mcell s in
                      let m = Mcell.union m1 m2 in
                      Mcell.clear_hashtbl Mcell.HUNION;
                      m
              end
          | Inter ->
              begin
                match !filename2 with
                  | None -> raise (Arg.Bad "Usage : -inter [filename1] [filename2]")
                  | Some s ->
                      let m1 = Common_fun.file_to_mcell filename1 in
                      let m2 = Common_fun.file_to_mcell s in
                      let m = Mcell.inter m1 m2 in
                      Mcell.clear_hashtbl Mcell.HINTER;
                      m
              end
          | Diff ->
              begin
                match !filename2 with
                  | None -> raise (Arg.Bad "Usage : -diff [filename1] [filename2]")
                  | Some s ->
                      let m1 = Common_fun.file_to_mcell filename1  in
                      let m2 = Common_fun.file_to_mcell s in
                      let m = Mcell.diff m1 m2 in
                      Mcell.clear_hashtbl Mcell.HDIFF;
                      m
              end
      in

  (* On itère le jeu de la vie si besoin selon les arguments passés *)
      let mcell =
        if !time > 0 then Mcell.iter ~t:!time mcell
        else mcell
      in

  (* On écrit dans un fichier le résultat de la mcell si on l'a voulu *)
      begin
        match !result_out with
          | None -> ();
          | Some(s) ->
              if !out_type = Rle then Rle.write mcell s
              else Mc_file.write mcell s
      end;

      begin
        match !filename_dot with
          | None -> ();
          | Some(s) ->
            Mcell.dot s mcell
      end;

      if !graphic then Guigol.main mcell
    end
  (* Sinon on execute l'ast contenu dans le fichier .mcop *)
  else
    let fd = Guigol.main in
    let frle = Rle.write in
    let fmc = Mc_file.write in
    Common_fun.execute_mcop filename1 fd frle fmc


let _ =
  let argvl = Array.length Sys.argv in
  if argvl = 1 then
    let mcell = Mcell.empty 3 in
    Guigol.main mcell
  else
    Arg.parse speclist main "Aie"

(* let main filename1 = *)
(*   let argvl = Array.length Sys.argv in *)
(*   if argvl = 1 then *)
(*     let mcell = Mcell.empty 3 in *)
(*     Guigol.main mcell *)
(*   else *)
(*     if (Sys.argv.(1) = "-test") then Test.lancer_test () *)
(*     else if (Sys.argv.(1) = "union") then *)
(*       if argvl <> 4 then *)
(*         print_endline "Usage: usage [filename1] [filename2]" *)
(*       else  *)
(*         let m1 = file_to_mcell Sys.argv.(2) in *)
(*         let m2 = file_to_mcell Sys.argv.(3) in *)
(*         Guigol.main (Mcell.union m1 m2) *)
(*     else  *)
(*       let file = Sys.argv.(1) in *)
(*       let mcell = file_to_mcell file in *)
(*       Guigol.main mcell *)
