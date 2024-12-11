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

let create_graph t tlist = 
  let max_win_t = t.wins + t.g_left in
  let rec f gr =

  match tlist with
  | [] -> gr
  | team :: rest -> if team.name!=t.name then gr


  in
  f empty_graph