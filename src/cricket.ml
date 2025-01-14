(*Ce fichier est dédié aux programmes permettant de résoudre le problème de cricket élimination.*)

open Algo
open Graph
open Tools
open Printf

(*Représente une équipe par son id, son nom, son nombre de victoires et le nombre de matchs qu'elle a encore 
à jouer.*)
type team = {
  idTeam: int;
  name: string ;
  wins: int ;
  g_left: int
}

(*Représente une rencontre par son id, l'id des équipes qui se rencontrent et le nombre de matchs encore à jouer 
entre ces équipes.*)
type matchs = {
  idMatch: int;
  idTeam1: int ;
  idTeam2: int ;
  remain: int
} 

(*Créé un graphe à partir de l'équipe étudiée, de la liste des équipes et de la liste des rencontres.
A partir du graphe créé et de l'algorithme de Ford-Ferguson, on peut déterminer si l'équipe étudiée peut encore
gagner le tournoi.*)
let create_graph t teams lmatchs = 
  let max_win_t = t.wins + t.g_left in
  let gr1 = new_node empty_graph 0 in
  let gr2 = new_node gr1 1 in
  let rec f igr teams1 =
    match teams1 with
    | [] -> igr
    | team :: rest -> if team.name!=t.name then 
      let igr1 = new_node igr team.idTeam in f (add_arc igr1 team.idTeam 1 (max_win_t-team.wins) (+)) rest else 
      f igr rest
  in
  let rec g igr lmatchs1 = 
    match lmatchs1 with
    | [] -> igr
    | match1 :: rest -> if match1.idTeam1!=t.idTeam && match1.idTeam2!=t.idTeam 
      then let igr1 = new_node igr match1.idMatch in let igr2 = add_arc igr1 0 match1.idMatch match1.remain (+) 
      in let igr3 =  add_arc igr2 match1.idMatch match1.idTeam1 max_int (+) in g (add_arc igr3 match1.idMatch match1.idTeam2 max_int (+)) rest
      else g igr rest
  in
  let gr3 = f gr2 teams in
  g gr3 lmatchs

(*A partir d'un graphe et de l'algorithme et à l'aide Ford-Ferguson, détermine si l'équipe étudiée peut
encore remporter le tournoi.*)
let resolution graph = 
  let resFF = ford_fulkerson2 graph 0 1 in
  let rec f arcs = 
    match arcs with
    | [] -> true
    | x::rest -> if x.lbl!=0 then false else f rest
  in 
  f (out_arcs resFF 0)

(*Permet de trouver une équipe dans une liste d'équipes à partir de son nom.*)
let rec find_team teams name = 
  match teams with
  | [] -> failwith "Team not found"
  | team::rest -> if team.name=name then team else find_team rest name  

(*Combine create_graph et resolution afin de résoudre le problème de cricket elimination.*)  
let cricket_elimination teams_and_matchs observed_team_name = 
  match teams_and_matchs with
  | ([],[]) -> failwith "No data"
  | (teams, matchs) -> let graph = create_graph (find_team teams observed_team_name) teams matchs in
   let solution = resolution graph in
   match solution with
   | true -> printf "%s can still win the tournament\n" observed_team_name; true
   | false -> printf "%s can no longer win the tournament\n" observed_team_name; false
