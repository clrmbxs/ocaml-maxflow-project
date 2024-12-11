open Graph
open Printf
open Cricket
    
(* Reads a line with a team. *)
let read_team teams line=
  try Scanf.sscanf line "t %s %d %d" (fun t w g-> {name=t; wins=w; g_left=g} :: teams)
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

let read_matchs lmatchs line=
  try Scanf.sscanf line "t %s %s %d" (fun t1 t2 m-> {team1=t1; team2=t2; remain=m} :: lmatchs)
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

let from_file path =

  let infile = open_in path in

  (* Read all lines until end of file. *)
  let rec loop season =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let season2 =
        (* Ignore empty lines *)
        if line = "" then season

        (* The first character of a line determines its content : n or e. *)
        else match (season,line.[0]) with
          | ((teams,lmatchs),'t') -> (read_team teams line,lmatchs)
          | ((teams,lmatchs),'m') -> (teams,read_matchs lmatchs line)

          (* It should be a comment, otherwise we complain. *)
          | _ -> season
      in      
      loop season2

    with End_of_file -> season (* Done *)
  in

  let final = loop ([],[]) in
  
  close_in infile ;
  final
  
