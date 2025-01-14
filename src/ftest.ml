open Gfile
open Tools
open Algo

(*A décommenter pour tester cricket elimination.*)
(*open Cricket
open Cfile*)
    
let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a graph\n" ^
         "    🟄  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    🟄  sink    : identifier of the sink vertex (ditto)\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in
  (* Open file *)
  let _graph = from_file infile in

  let _f lbl = int_of_string lbl in
  let _f1 lbl = string_of_int lbl in
  let _f2 = function
    |(a,b) -> string_of_int a^"/"^string_of_int b
  in

  (*A commenter pour tester cricket elimination.*)
  (*Début*)
  let graph2 = gmap _graph _f in
  let graph2 = ford_fulkerson graph2 _source _sink (initialize graph2) in
  Printf.printf "Flot max : %d \n%!" (flot graph2 _source); 

  let () = export outfile (gmap graph2 _f2) in
  (*Fin*)
  
  (*A décommenter pour tester cricket elimination.*)

  (*let obsevrved_team = "CSK" in
  let (teams,matchs) = from_file_cricket "./graphs/cricket.txt" in
  let graph = create_graph (find_team teams obsevrved_team) teams matchs in
  let sol = cricket_elimination (from_file_cricket "./graphs/cricket.txt") obsevrved_team in
  Printf.printf "%b\n" (sol);

  let () = export outfile (gmap graph _f1) in*)
  

  ()

