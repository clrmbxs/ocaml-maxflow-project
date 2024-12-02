open Graph
open Tools
let initialize gr1 = gmap gr1 (fun x -> (0,x)) 

let rec find_path gr id1 id2 acu =
  
  
  
  
  
  let explore arcs = match arcs with
  | [] -> []
  | arc::rest -> if arc.tgt=id2 then acu else find_path gr arc.tgt id2 (arc::acu) 
match out_arcs gr id1 with 
  | [] -> None
  | arc::rest -> if arc.tgt=id2 then Some acu else find_path gr arc.tgt id2 (arc::acu)
