
let filename = Sys.argv.(1)
let mc = Rle.read filename
let mc = Mcell.empty 20
let () = Mcell.dot ~filename:"mcell.dot" mc
