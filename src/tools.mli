open Graph

val clone_nodes: 'a graph -> 'b graph
val gmap: 'a graph -> ('a -> 'b) -> 'b graph
val add_arc: 'a graph -> id -> id -> 'a -> ('a -> 'a -> 'a)-> 'a graph
val construire: int graph -> int arc list -> int graph
val (++): (int*int) -> (int*int) -> (int*int)