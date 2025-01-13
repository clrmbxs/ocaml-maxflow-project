open Graph

type team = {
  idTeam: int;
  name: string ;
  wins: int ;
  g_left: int
}
type matchs = {
  idMatch: int;
  idTeam1: int ;
  idTeam2: int ;
  remain: int
} 

val create_graph: team -> team list -> matchs list -> int graph
val resolution: int graph -> bool
val find_team: team list -> string -> team
val cricket_elimination: team list*matchs list -> string -> bool