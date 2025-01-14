(*Ce fichier est dédié à l'algorithme de Ford-Ferguson.*)

open Graph
open Tools

(*Transforme un int graphe en int*int graphe (graphe des flots initialisé).*)
let initialize gr1 = gmap gr1 (fun x -> (0,x)) 

(*Trouve un chemin dans un graphe entre deux noeuds.*)
(*existe est déclaré dans tools.*)
let rec find_path gr id1 id2 acu =
  let rec explore arcs = match arcs with
    | [] -> []
    | arc::rest -> 
      if arc.tgt=id2 && arc.lbl!=0 then arc::acu else 
        if existe arc acu || arc.lbl=0 then explore rest else
      let res1=find_path gr arc.tgt id2 (arc::acu) in match res1 with
      | [] -> explore rest
      | _ -> res1
  in
  explore (out_arcs gr id1)

(*Retourne le flot minimal sur un chemin.*)
let rec min_flow = function
  | [] -> max_int
  | arc::rest -> min arc.lbl (min_flow rest)

(*Met à jour le un graphe à partir en augmentant le flot de n sur un chemin (liste d'arcs): version int*int graph.*)
(*(++) est déclaré dans tools*)
let rec augmentationT gr n = function
  | [] -> gr
  | arc::rest -> match find_arc gr arc.src arc.tgt with
    |None -> augmentationT (add_arc gr arc.tgt arc.src (-n,0) (++)) n rest
    |Some _ -> augmentationT (add_arc gr arc.src arc.tgt (n,0) (++)) n rest

(*Met à jour le un graphe à partir en augmentant le flot de n sur un chemin (liste d'arcs): version int graph.*) 
let rec augmentation gr n = function
| [] -> gr
| arc::rest -> augmentation (add_arc (add_arc gr arc.tgt arc.src n (+)) arc.src arc.tgt n (-)) n rest 

(*Algorithme de Ford-Ferguson: version int graph.*)
let rec ford_fulkerson2 gr id1 id2=
  match find_path gr id1 id2 [] with
    | [] -> gr
    | arcs -> Printf.printf "Min : %d \n %!" (min_flow arcs); 
    ford_fulkerson2 (augmentation gr (min_flow arcs) arcs) id1 id2 
  
(*Algorithme de Ford-Ferguson: version int*int graph -> ce type est utilisé pour un meilleur affichage du graphe.*)
let rec ford_fulkerson gr id1 id2 acu =
  match find_path gr id1 id2 [] with
    | [] -> acu
    | arcs -> ford_fulkerson (augmentation gr (min_flow arcs) arcs) id1 id2  (augmentationT acu (min_flow arcs) arcs)

(*Retourne le flot d'un graph (de type int*int graph).*)
let flot gr id =
  let f acu arc = 
    match arc.lbl with
    | (n,_) -> n + acu in 
    List.fold_left f 0 (out_arcs gr id) 

