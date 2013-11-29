
open Mcell
open Hmatrix

let mcell_test_1 = create on on off off
let mcell_test_2 = create mcell_test_1 mcell_test_1 mcell_test_1 mcell_test_1

let empty_cell = create off off off off
let full_cell = create on on on on

let lancer_test () =
  (* Verification pour empty *)
  assert (is_empty empty_cell);
  assert (size empty_cell = 1);
  assert (empty_cell.ne == off);

  (* Vérification des fonctions de Hmatrix *)
  let matrix = mcell_to_array mcell_test_1 in
  let mc = array_to_mcell matrix in
  assert (mc == mcell_test_1);

  (* Vérification sur les tailles *)
  assert (size mcell_test_1 = 1);
  assert (size mcell_test_1 + 1 = size mcell_test_2);
 
 Format.eprintf "Fin des tests@."
  

