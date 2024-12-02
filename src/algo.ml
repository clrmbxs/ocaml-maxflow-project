open Graph
open Tools
let _initialize gr1 = gmap gr1 (fun x -> (0,x)) 

let rec find_path gr id1 id2 acu =
  let rec explore arcs = match arcs with
    | [] -> []
    | arc::rest -> if arc.tgt=id2 then arc::acu else let res1=find_path gr arc.tgt id2 (arc::acu) in match res1 with
      | [] -> explore rest
      | _ -> res1
  in
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

let rec augmentation gr n = function
  | [] -> gr
  | arc::rest -> augmentation (add_arc gr arc.src arc.tgt n (++)) n rest



  let ford_fulkerson _gr _id1 _id2=assert false 

