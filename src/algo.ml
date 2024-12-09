open Graph
open Tools
let initialize gr1 = gmap gr1 (fun x -> (0,x)) 

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
  Printf.printf "Je visite : %d \n %!" (id1);
  explore (out_arcs gr id1)
  

  (*match out_arcs gr id1 with 
  | [] -> []
  | arc::rest -> let res=explore (arc::rest) in match res with
    | [] -> explore rest
    | _ -> res*)
      (*if explore (arc::rest)!=[] then explore (arc::rest) else explore rest*)



let rec min_flow = function
  | [] -> max_int
  | arc::rest -> min arc.lbl (min_flow rest)

  
let rec augmentationT gr n = function
  | [] -> gr
  | arc::rest -> match find_arc gr arc.src arc.tgt with
    |None -> augmentationT (add_arc gr arc.tgt arc.src (-n,0) (++)) n rest
    |Some _ -> augmentationT (add_arc gr arc.src arc.tgt (n,0) (++)) n rest
  
let rec augmentation gr n = function
| [] -> gr
| arc::rest -> augmentation (add_arc (add_arc gr arc.tgt arc.src n (+)) arc.src arc.tgt n (-)) n rest 

  let rec ford_fulkerson gr id1 id2=
    match find_path gr id1 id2 [] with
      | [] -> gr
      | arcs -> Printf.printf "Min : %d \n %!" (min_flow arcs); 
      ford_fulkerson (augmentation gr (min_flow arcs) arcs) id1 id2 
  

    let rec ford_fulkerson2 gr id1 id2 acu =
    match find_path gr id1 id2 [] with
      | [] -> acu
      | arcs -> Printf.printf "Min : %d \n %!" (min_flow arcs); 
      ford_fulkerson2 (augmentation gr (min_flow arcs) arcs) id1 id2  (augmentationT acu (min_flow arcs) arcs)

(*let rec ford_fulkerson2 gr id1 id2 =
  let res = ford_fulkerson gr id1 id2 in
  gmap gr1 (fun arc -> if (find_arc res arc.tgt arc.src) then*)