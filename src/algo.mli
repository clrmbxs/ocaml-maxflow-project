open Graph

val ford_fulkerson: int graph -> int -> int -> (int*int) graph -> (int*int) graph
val initialize: int graph -> (int*int) graph
val flot: (int*int) graph -> int -> int

val ford_fulkerson2: int graph -> int -> int -> int graph
val find_path: int graph -> int -> int -> int arc list -> int arc list
val min_flow: int arc list -> int
val augmentationT: (int*int) graph -> (int) -> 'a arc list -> (int*int) graph
val augmentation: (int) graph -> (int) -> 'a arc list -> (int) graph
