open Gfile
open Tools
open Algo
open Cricket
open Cfile
    
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
  and source = int_of_string Sys.argv.(2)
  and sink = int_of_string Sys.argv.(3)
  in
  (* Open file *)
  let graph = from_file infile in

  let f lbl = int_of_string lbl in
  let _f1 lbl = string_of_int lbl in
  (*let f2 = function
    |(a,b) -> string_of_int a^"/"^string_of_int b
  in*)

  let graph2 = gmap graph f in
  let graph2 = ford_fulkerson graph2 source sink (initialize graph2) in
  Printf.printf "Flot max : %d \n %!" (flot graph2 source); 
  (*let graph2 = gmap graph2 f2 in*)

  let teams={idTeam=2; name="MI"; wins=83; g_left=8}::{idTeam=3; name="CSK"; wins=80; g_left=3}::{idTeam=4; name="KKR"; wins=78; g_left=6}::{idTeam=5; name="DC"; wins=77; g_left=3}::[] in
  let matchs={idMatch=23; idTeam1=2; idTeam2=3; remain=1}::{idMatch=24; idTeam1=2; idTeam2=4; remain=6}::{idMatch=25; idTeam1=2; idTeam2=5; remain=1}::{idMatch=35; idTeam1=3; idTeam2=5; remain=2}::[] in
  let graph = create_graph (find_team teams "CSK") teams matchs in
  let sol = cricket_elimination (from_file_cricket "./graphs/cricket.txt") "CSK" in
  Printf.printf "%b\n" (sol);
  (* Rewrite the graph that has been read. *)
  let () = export outfile (gmap graph _f1) in

  ()

