(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let clone_nodes gr = n_fold gr new_node empty_graph
let gmap gr f = 
  let a arc = {src = arc.src ; tgt = arc.tgt ; lbl = f arc.lbl} in
  let t graph arc = new_arc graph (a arc) in
  e_fold gr t (clone_nodes gr)

let add_arc gr id1 id2 n f= 
  match find_arc gr id1 id2 with
  | None -> let arc = {src = id1 ; tgt = id2 ; lbl = n} in new_arc gr arc
  | Some arc1 -> let arc = {src = id1 ; tgt = id2 ; lbl = (f n arc1.lbl)} in new_arc gr arc

let (++) op1 op2 = 
  match op1 with
    (a,b) -> match op2 with
    | (c,d) -> (a+c,b+d)  
(* Replace _gr and _f by gr and f when you start writing the real function. *)

let rec construire gr arcs = 
  match arcs with
    | [] -> gr
    | arc::rest -> construire (add_arc gr arc.src arc.tgt arc.lbl (+)) rest