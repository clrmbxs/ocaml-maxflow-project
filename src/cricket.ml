open Algo
open Graph
open Tools

type team = {
  name: string ;
  wins: int ;
  g_left: int
}
type matchs = {
  team1: string ;
  team2: string ;
  remain: int
} 

let create_graph t teams lmatchs = 
  let max_win_t = t.wins + t.g_left in
  let rec f gr teams1 id =

  match teams1 with
  | [] -> gr
  | team :: rest -> if team.name!=t.name then f (new_node gr (id)) rest (id+1) else f gr rest (id)


  in
  f empty_graph teams 2