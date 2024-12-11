open Gfile
open Tools
open Algo
    
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
  let f2 = function
    |(a,b) -> string_of_int a^"/"^string_of_int b
  in

  let graph2 = gmap graph f in
  let graph2 = ford_fulkerson graph2 source sink (initialize graph2) in
  Printf.printf "Flot max : %d \n %!" (flot graph2 source); 
  let graph2 = gmap graph2 f2 in

  (* Rewrite the graph that has been read. *)
  let () = export outfile graph2 in

  ()

