(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let clone_nodes gr = n_fold gr new_node empty_graph
let gmap gr f = 
  let a arc = {src = arc.src ; tgt = arc.tgt ; lbl = f arc.lbl} in
  let t graph arc = new_arc graph (a arc) in
  e_fold gr t (clone_nodes gr)




let add_arc _gr _id1 _id2 _n = assert false
(* Replace _gr and _f by gr and f when you start writing the real function. *)