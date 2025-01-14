(*Ce fichier est dédié à la traduction d'un fichier contenant les caractéristiques de la saison étudiée
dans le cadre du problème de cricket elimination, en une liste d'équipes et une liste de rencontres avec les
données nécessaires à la résolution du problème.*)

open Cricket
    
(* Reads a line with a team and adds it to the list.*)
let read_team teams line=
  try Scanf.sscanf line "t %d %s %d %d" (fun it t w g-> {idTeam=it; name=t; wins=w; g_left=g} :: teams)
  with e ->
    Printf.printf "Cannot read team in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file_cricket"

(* Reads a line with a match and adds it to the list.*)
let read_matchs lmatchs line=
  try Scanf.sscanf line "m %d %d %d %d" (fun im it1 it2 m-> {idMatch=im; idTeam1=it1; idTeam2=it2; remain=m} :: lmatchs)
  with e ->
    Printf.printf "Cannot read match in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file_cricket"

(*A l'image de from_file dans gfile, from_file_cricket traduit un fichier d'une configuration précise en données
utiles: ici une liste de'équipes et de matchs.*)
let from_file_cricket path =

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

        (* The first character of a line determines its content : t or m. *)
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
  
