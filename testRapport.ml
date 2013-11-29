
open Mcell

let testCarpet n =
  let m = carpet n in
  let cnt_nodes = count_nodes m in
  let id_before = m.id in
  let timeStart = Sys.time () in
  let m = result (carpet n) in
  let time = Sys.time () -. timeStart in
  let id_diff = m.id - id_before in
  
  let out = open_out ("tests/testsCarpet"^(string_of_int n)^".csv") in 
  let s = Format.sprintf "%d,%f,%d,%d\n"
    n time cnt_nodes id_diff in
  output_string out s;
  close_out out

let testGlider t =
  let m = Rle.read "examples/glider.rle" in
  
  let id_before = m.id in
  let timeStart = Sys.time () in
  let m = iter ~t m in
  let time = Sys.time() -. timeStart in
  let id_diff = m.id - id_before in

  let s = Format.sprintf "%d,%d,%f,%d\n"
    (Mcell.size m) t time id_diff in
  let out = open_out ("tests/testsGlider"^(string_of_int t)^".csv") in
  output_string out s;
  close_out out

let testFile f name =
  let time1 = Sys.time () in
  let m = Common_fun.file_to_mcell f in
  let time2 = Sys.time () in
  let nb_cell_lives = Mcell.count_live_cells m in
  let nb_nodes = Mcell.count_nodes m in
  let time3 = Sys.time () in
  let m = result m in
  let time4 = Sys.time () in
  let s = Format.sprintf "%d,%d,%d,%f,%f\n" 
    (m.size) nb_cell_lives nb_nodes (time2-.time1)
    (time4-.time3) in

  let out = open_out ("tests/tests_"^name^".csv") in
  output_string out s;
  close_out out

let mergeCarpetsFiles () =
  ignore (Sys.command "rm -f tests/allCarpets.csv");
  ignore (Sys.command "cat tests/testsCarpet* >> tests/allCarpets.csv")

let mergeGlidersFiles () =
  ignore (Sys.command "rm -f tests/allGliders.csv");
  ignore (Sys.command "cat tests/testsGlider* >> tests/allGliders.csv")

let mergeTestsFiles () =
  ignore (Sys.command "rm -f tests/allFiles.csv");
  ignore (Sys.command "cat tests/tests_* >> tests/allFiles.csv")

let testTicker p =
  let m = ref (Common_fun.file_to_mcell "files/ticker.rle") in
  let buf = Buffer.create 5003 in

  let nb_node = count_nodes !m in
  let s = Format.sprintf "0,%d,0.00000\n" nb_node in
  Buffer.add_string buf s;
  
  for i=1 to 1000 do
    let time1 = Sys.time () in
    let t = i*p in
    m := Mcell.iter ~t:p !m;
    let time = Sys.time () -. time1 in
    let hsize = Mcell.size_hashtbl HCREATE in
    let s = Format.sprintf "%d,%d,%f\n" t hsize time in
    Buffer.add_string buf s;
  done;

  let fo = Format.sprintf "tests/testGraphTicker_p%d.csv" p in
  let out = open_out fo in
  output_string out (Buffer.contents buf);
  close_out out


let _ =
  let args = Sys.argv in
  if args.(1) = "-c" then
    (testCarpet (int_of_string args.(2));
     mergeCarpetsFiles ())
  else if args.(1) = "-g" then
    (testGlider (int_of_string args.(2));
     mergeGlidersFiles ())
  else if args.(1) = "-ac" then
    begin
      for i=1 to 30 do
	let n = 2*i in
	ignore (Sys.command
	   ("./testRapport -c "^(string_of_int n)))
      done;
    end
  else if args.(1) = "-ag" then
    begin
      for i=0 to 30 do
	let n = 1 lsl i in
	ignore (Sys.command
	   ("./testRapport -g "^(string_of_int n)))
      done;
    end
  else if args.(1) = "-tt" then
    testTicker (int_of_string args.(2))
  else if args.(1) = "-memc" then
    let m = Mcell.carpet (int_of_string args.(2)) in
    let filename = Format.sprintf "testMemoryCarpet%s.dot" args.(2) in
    dot ~filename m
  else (testFile args.(1) args.(2);
	mergeTestsFiles ())
