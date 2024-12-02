open Graph

val ford_fulkerson: int graph -> int -> int -> (int*int) graph * int
val find_path: int graph -> int -> int -> int arc list -> int arc list
val min_flow: int arc list -> int
val augmentation: (int*int) graph -> (int*int) -> 'a arc list -> (int*int) graph
